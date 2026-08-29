;;;; tools/gen-bindings.lisp — draft the src/ffi/ layer from IM's headers.
;;;;
;;;; NOT part of any shipped system. Run by hand when upstream changes:
;;;;
;;;;   sbcl --non-interactive --load tools/gen-bindings.lisp \
;;;;        --eval '(im.gen:generate "/path/to/tecgraf-im")'
;;;;
;;;; What comes out is a first draft, not a finished binding. Every file it
;;;; writes is committed and then hand-corrected, and the corrections stay --
;;;; regenerating overwrites, so re-run it into a clean tree and diff.
;;;;
;;;; Two design choices worth stating:
;;;;
;;;; The symbol list is taken from the BUILT LIBRARIES, not from the headers.
;;;; Headers declare things no library exports: imFormatRegisterAVI and
;;;; imFormatRegisterWMV were declared and documented for years with no CMake
;;;; target behind them, and the previous binding dutifully bound all three of
;;;; those plus imCompressDataLZO, which upstream had replaced with LZ4. Each
;;;; was a function that existed until you called it. Driving from nm(1) makes
;;;; that class of bug impossible to introduce.
;;;;
;;;; Docstrings come from the doxygen comments. IM's headers document every
;;;; public function, and re-typing that by hand into 465 defcfun forms would
;;;; be both enormous and worse.

(require :asdf)
(asdf:load-system :cl-ppcre)
(asdf:load-system :alexandria)

(defpackage #:im.gen
  (:use #:common-lisp)
  (:export #:generate #:coverage-report))

(in-package #:im.gen)

;;; ---------------------------------------------------------------------------
;;; Name conversion
;;; ---------------------------------------------------------------------------

(defparameter *name-fixups*
  '(("FFTraw"  . "FftRaw")
    ("UShort"  . "Ushort")
    ("NLen"    . "Nlen")
    ("RGB2Map" . "Rgb2Map")
    ("Map2RGB" . "Map2Rgb")
    ("RGB2Gray" . "Rgb2Gray")
    ("Map2Gray" . "Map2Gray"))
  "C spellings whose camel-case boundaries the general rule gets wrong.

The rule that turns HTTPServer into http-server -- break before a capital that
is followed by a lowercase letter -- needs the second word to be capitalised.
IM has several names where it is not: imProcessFFTraw is FFT + raw, but the
rule sees FF|Traw and emits %im-process-ff-traw. No amount of cleverness
recovers the intended split without knowing that \"raw\" is a word, so the
handful of affected names are listed instead.")

(defun apply-name-fixups (name)
  (let ((result name))
    (dolist (fix *name-fixups* result)
      (let ((at (search (car fix) result)))
        (when at
          (setf result (concatenate 'string
                                    (subseq result 0 at)
                                    (cdr fix)
                                    (subseq result (+ at (length (car fix)))))))))))

(defun kebab (name &key (strip-im nil))
  "Convert a C identifier to kebab case.

STRIP-IM drops a leading \"im\", which is right for function names -- the
prefix is C's namespace and the Lisp package is ours -- and wrong for
everything else. Applied to parameter names it turned `image` into `age`,
`image1` into `age1`, and produced signatures that read (age im-image).

Runs of capitals stay together, so RGB does not become r-g-b."
  (let* ((name (apply-name-fixups name))
         (s (if (and strip-im (> (length name) 2) (string= "im" (subseq name 0 2)))
                (subseq name 2)
                name))
         (out (make-string-output-stream)))
    (loop for i below (length s)
          for c = (char s i)
          for prev = (when (plusp i) (char s (1- i)))
          for next = (when (< (1+ i) (length s)) (char s (1+ i)))
          do (when (and prev
                        (upper-case-p c)
                        (or (lower-case-p prev)
                            (digit-char-p prev)
                            (and next (lower-case-p next))))
               (write-char #\- out))
             (write-char (char-downcase c) out))
    (get-output-stream-string out)))

(defun %split-underscore (s)
  (let (parts (start 0))
    (loop for i = (position #\_ s :start start)
          do (push (subseq s start i) parts)
             (if i (setf start (1+ i)) (return)))
    (nreverse parts)))

(defun common-member-prefix (members)
  "The longest underscore-delimited prefix shared by every member name.

IM_ERR_NONE / IM_ERR_OPEN / ... -> \"IM_ERR_\", so the keyword becomes
:ERROR-CODE-NONE rather than :ERROR-CODE-ERR-NONE. For imDataType the shared
prefix is just \"IM_\", giving :DATA-TYPE-BYTE. Reproducing the convention the
hand-written binding already used, without a table of special cases.

Never consumes a member entirely: an enum whose members are IM_A and IM_A_B
would otherwise leave the first one nameless."
  (when (null members) (return-from common-member-prefix ""))
  (let* ((split (mapcar #'%split-underscore members))
         (shortest (reduce #'min split :key #'length))
         (n 0))
    ;; Count how many leading underscore-delimited components every member
    ;; agrees on, stopping one short of the shortest member so nothing is
    ;; emptied.
    (loop for i below (1- shortest)
          for candidate = (nth i (first split))
          while (every (lambda (parts) (equal (nth i parts) candidate)) split)
          do (incf n))
    (if (zerop n)
        ""
        (format nil "~{~A_~}" (subseq (first split) 0 n)))))

(defun enum-keyword (enum-lisp-name member prefix)
  "IM_ERR_OPEN with prefix IM_ERR_ under error-code -> :ERROR-CODE-OPEN."
  (let ((tail (if (and (plusp (length prefix))
                       (alexandria:starts-with-subseq prefix member))
                  (subseq member (length prefix))
                  member)))
    (intern (string-upcase
             (format nil "~A-~A" enum-lisp-name
                     (substitute #\- #\_ (string-downcase tail))))
            :keyword)))

(defun enum-lisp-name (c-name)
  "imErrorCodes -> error-code. Singularised, because the members are the
plural and the type is one of them."
  (let ((k (kebab c-name :strip-im t)))
    (cond ((alexandria:ends-with-subseq "codes" k)
           (concatenate 'string (subseq k 0 (- (length k) 5)) "code"))
          ((and (alexandria:ends-with-subseq "s" k)
                (not (alexandria:ends-with-subseq "ss" k)))
           (subseq k 0 (1- (length k))))
          (t k))))

;;; ---------------------------------------------------------------------------
;;; Type mapping
;;; ---------------------------------------------------------------------------

(defparameter *scalar-types*
  '(("void"               . :void)
    ("int"                . :int)
    ("unsigned int"       . :unsigned-int)
    ("char"               . :char)
    ("signed char"        . :char)
    ("unsigned char"      . :unsigned-char)
    ("short"              . :short)
    ("unsigned short"     . :unsigned-short)
    ("long"               . :long)
    ("unsigned long"      . :unsigned-long)
    ("float"              . :float)
    ("double"             . :double)
    ("size_t"             . :size)))

(defparameter *opaque-types*
  '(("imFile"         . im-file)
    ("imImage"        . im-image)
    ("imBinFile"      . im-bin-file)
    ("imVideoCapture" . im-video-capture)
    ("imAttribTablePrivate" . im-attrib-table))
  "IM's opaque handles. Each gets a DEFCTYPE aliasing :POINTER in
src/ffi/types.lisp, so a signature reads imImage rather than :pointer and a
mistake is visible.")

(defparameter *callback-types*
  '("imAttribTableCallback" "imBinFileNewFunc" "imCallback" "imCounterCallback"
    "imDibLineGetPixelFunc" "imDibLineSetPixelFunc" "imFileCounterCallback"
    "imMultiPointColorOpFunc" "imMultiPointOpFunc" "imRenderCondFunc"
    "imRenderFunc" "imResolutionCallback" "imUnaryPointColorOpFunc"
    "imUnaryPointOpFunc")
  "Function-pointer typedefs.

:POINTER is the right and only answer for these -- a Lisp callback reaches C
as the pointer CFFI:CALLBACK returns -- so they are listed rather than left to
fall through to the unmapped case. Without this every function taking a
callback carried a REVIEW marker forever, which trains the reader to ignore
the markers that do mean something.")

(defparameter *enum-types* (make-hash-table :test #'equal)
  "C enum name -> lisp defcenum name, filled in as headers are parsed.")

(defparameter *ignored-type-tokens* '("IM_DECL")
  "Macros that appear inside a declaration's type but are not part of it.

IM_DECL (include/im_capture.h:15) expands to __cdecl on Windows and to nothing
elsewhere, and it sits between the return type and the function name:

    int IM_DECL imVideoCaptureDeviceCount(void);

Left in place it makes the return type read \"int IM_DECL\", which matches no
entry in the type table, so all 27 capture functions were emitted as
:pointer with a REVIEW marker -- including the ones returning int, whose
values would then have been read as addresses.")

(defun strip-ignored-tokens (s)
  (let ((result s))
    (dolist (token *ignored-type-tokens* result)
      (setf result (cl-ppcre:regex-replace-all
                    (format nil "\\b~A\\b" token) result " ")))))

(defun normalize-type (s)
  ;; The character bag must be a list of characters, not " \t". Common Lisp
  ;; string literals have no \t escape -- a backslash there just quotes the
  ;; next character -- so " \t" is the two-element bag {space, t}, and
  ;; STRING-TRIM was stripping a trailing letter t from every type it saw.
  ;; "const char* format" came back as "const char* forma", and the parameter
  ;; was bound under that name.
  (string-trim '(#\Space #\Tab)
               (cl-ppcre:regex-replace-all "\\s+" (strip-ignored-tokens s) " ")))

(defun map-type (raw &key returnp)
  "Map a C type string to a CFFI type, or a TODO marker for review.

RETURNP matters for one case only: a bare `char*` return is a string IM owns
and the caller must not free, which :STRING handles correctly. As a parameter
the same spelling is usually an output buffer."
  (let* ((s (normalize-type raw))
         (constp (search "const " s))
         (base (normalize-type (cl-ppcre:regex-replace-all "\\bconst\\b" s "")))
         (stars (count #\* base))
         (bare (normalize-type (remove #\* base))))
    (cond
      ;; const char* is always an input string. Non-const char* as a parameter
      ;; is an output buffer -- imFormatInfo fills desc and ext this way -- and
      ;; must stay a pointer, or CFFI would try to convert a Lisp string in and
      ;; nothing would come back out.
      ((and (string= bare "char") (= stars 1) (or constp returnp)) :string)
      ((and (string= bare "char") (= stars 1)) :pointer)
      ((and (string= bare "void") (zerop stars)) :void)
      ((plusp stars)
       (let ((opaque (cdr (assoc bare *opaque-types* :test #'string=))))
         (if (and opaque (= stars 1))
             opaque
             :pointer)))
      ((member bare *callback-types* :test #'string=) :pointer)
      ((gethash bare *enum-types*) (gethash bare *enum-types*))
      ((cdr (assoc bare *scalar-types* :test #'string=))
       (cdr (assoc bare *scalar-types* :test #'string=)))
      (t (list :todo raw)))))

;;; ---------------------------------------------------------------------------
;;; Header parsing
;;; ---------------------------------------------------------------------------

(defun read-header (path)
  "Read PATH as Latin-1. include/im_format_all.h is not valid UTF-8."
  (with-open-file (in path :external-format :latin-1)
    (let ((s (make-string (file-length in))))
      (subseq s 0 (read-sequence s in)))))

(defparameter +doc-scanner+
  (cl-ppcre:create-scanner "/\\*\\*(.*?)\\*/" :single-line-mode t))

(defparameter +decl-scanner+
  ;; A return type (possibly several words and stars), then an im* name, then
  ;; a parenthesised parameter list, then a semicolon. Anything containing a
  ;; brace is a definition, not a declaration, and is excluded.
  (cl-ppcre:create-scanner
   "([A-Za-z_][A-Za-z0-9_]*(?:\\s+[A-Za-z_][A-Za-z0-9_]*)*\\s*\\**)\\s*\\b(im[A-Za-z0-9_]+)\\s*\\(([^;{}]*?)\\)\\s*;"
   :single-line-mode t))

(defparameter +enum-scanner+
  (cl-ppcre:create-scanner "enum\\s+(im[A-Za-z0-9_]*)\\s*\\{([^}]*)\\}\\s*;"
                           :single-line-mode t))

(defun clean-doc (text)
  "Turn a doxygen block into a docstring.

Drops the Lua signature lines (\\verbatim ... \\endverbatim), the \\ingroup
tag and the leading asterisks, and flattens \\ref and \\n. What is left is
IM's own prose, which is the part worth keeping."
  (when text
    (let* ((s (cl-ppcre:regex-replace-all "(?s)\\\\verbatim.*?\\\\endverbatim" text ""))
           (s (cl-ppcre:regex-replace-all "\\\\ingroup\\s+\\S+" s ""))
           (s (cl-ppcre:regex-replace-all "\\\\(brief|par)\\s*" s ""))
           (s (cl-ppcre:regex-replace-all "\\\\ref\\s+" s ""))
           (s (cl-ppcre:regex-replace-all "\\\\n\\b" s ""))
           (s (cl-ppcre:regex-replace-all "(?m)^\\s*\\*+\\s?" s ""))
           (s (cl-ppcre:regex-replace-all "\\s+" s " ")))
      (setf s (string-trim " " s))
      (when (plusp (length s)) s))))

(defstruct (cdecl (:conc-name decl-))
  name return params doc header)

(defun parse-params (text)
  "Split a parameter list into (lisp-name . cffi-type) pairs."
  (let ((text (normalize-type text)))
    (when (or (string= text "") (string= text "void"))
      (return-from parse-params nil))
    (loop for raw in (cl-ppcre:split "\\s*,\\s*" text)
          for i from 0
          collect (let* ((raw (normalize-type raw))
                         ;; Trailing identifier is the parameter name, unless
                         ;; the declaration omitted one.
                         (m (nth-value 1 (cl-ppcre:scan-to-strings
                                          "^(.*?[\\s\\*])([A-Za-z_][A-Za-z0-9_]*)\\s*(\\[\\s*\\])?$"
                                          raw))))
                    (if m
                        (let ((type (aref m 0))
                              (name (aref m 1))
                              (array (aref m 2)))
                          (cons (kebab (substitute #\- #\_ name))
                                (if array :pointer (map-type type))))
                        (cons (format nil "arg~D" i) (map-type raw)))))))

(defun parse-enums (text)
  "Every `enum imFoo { ... };` in TEXT, as (c-name lisp-name members)."
  (let (result)
    (cl-ppcre:do-register-groups (name body) (+enum-scanner+ text)
      (let* ((body (cl-ppcre:regex-replace-all "(?s)/\\*.*?\\*/" body ""))
             (members
               (loop for chunk in (cl-ppcre:split "\\s*,\\s*" body)
                     for trimmed = (string-trim '(#\Space #\Tab #\Newline #\Return) chunk)
                     when (plusp (length trimmed))
                       collect (let ((eq (position #\= trimmed)))
                                 (if eq
                                     (cons (string-trim " " (subseq trimmed 0 eq))
                                           (string-trim " " (subseq trimmed (1+ eq))))
                                     (cons trimmed nil))))))
        (when members
          (push (list name (enum-lisp-name name) members) result))))
    (nreverse result)))

(defparameter +comment-scanner+
  ;; Comments, preprocessor directives (including backslash continuations),
  ;; and calling-convention macros.
  ;;
  ;; IM_DECL has to go before the declaration scanner runs, not after. It sits
  ;; between the return type and the function name:
  ;;
  ;;     const char* IM_DECL imVideoCaptureDeviceDesc(int device);
  ;;
  ;; and the declaration pattern cannot match across it -- IM_DECL is not a
  ;; pointer, and it is not the im-prefixed name either. So the scan starts
  ;; over AT "IM_DECL", captures that as the return type, and the real one is
  ;; gone before any amount of later cleaning could recover it. Blanking it
  ;; here leaves "const char*  imVideoCaptureDeviceDesc(...)", which parses.
  (cl-ppcre:create-scanner
   "(?s)/\\*.*?\\*/|//[^\\n]*|(?m:^[ \\t]*#(?:[^\\n\\\\]|\\\\.)*)|\\bIM_DECL\\b"))

(defun blank-comments (text)
  "A copy of TEXT with every comment and preprocessor line replaced by spaces.

Offsets are preserved so doc comments found in the original still line up with
declarations found in the blanked copy.

This is not tidiness, it is correctness. The declaration scanner allows a
parameter list to span newlines, which real C declarations do. Left to run
over comment text it will happily start at an English word, take a
parenthesis from a sentence like \"they do not use imBinFile (like AVI and
WMV)\", and then scan forward for the first `);` -- which is the closing paren
of the NEXT real declaration. That match swallows the declaration whole, and
because matches do not overlap, the function silently disappears from the
output. imFileHandle, imColorModeToBitmap, imBinSwapBytes, imCalcRMSError,
imProcessUnsharp and five others were lost exactly this way.

Function-like macros do the same damage and are blanked for the same reason:
`#define imColorModeIsTopDown(_cm) (_cm & IM_TOPDOWN)` offers a name, a
parenthesis and no semicolon, so the scan ran on into the next real
declaration and ate imColorModeToBitmap."
  (let ((out (copy-seq text)))
    (cl-ppcre:do-matches (ms me +comment-scanner+ text)
      (loop for i from ms below me
            ;; Keep newlines so line-oriented anchors still behave.
            unless (char= (char out i) #\Newline)
              do (setf (char out i) #\Space)))
    out))

(defun parse-decls (text header)
  (let ((docs '())
        (code (blank-comments text)))
    (cl-ppcre:do-matches (ms me +doc-scanner+ text)
      (push (cons me (clean-doc (subseq text (+ ms 3) (- me 2)))) docs))
    (setf docs (nreverse docs))
    (let (result)
      (cl-ppcre:do-matches (ms me +decl-scanner+ code)
        (declare (ignore me))
        (multiple-value-bind (match groups)
            (cl-ppcre:scan-to-strings +decl-scanner+ code :start ms)
          (declare (ignore match))
          (let* ((ret (aref groups 0))
                 (name (aref groups 1))
                 (params (aref groups 2))
                 ;; The doc comment for a declaration is the last one ending
                 ;; before it starts -- but only if nothing but whitespace
                 ;; separates the two. Without that check a declaration with no
                 ;; doxygen block of its own silently adopts the prose of
                 ;; whatever was documented above it, which is worse than
                 ;; having no docstring: it is a confident wrong answer
                 ;; attached to a function it does not describe.
                 (doc (let ((candidate (car (last (remove-if (lambda (d) (> (car d) ms))
                                                             docs)))))
                        (when (and candidate
                                   (every (lambda (c)
                                            (member c '(#\Space #\Tab #\Newline #\Return)))
                                          (subseq code (car candidate) ms)))
                          (cdr candidate)))))
            (push (make-cdecl :name name
                              :return (map-type ret :returnp t)
                              :params (parse-params params)
                              :doc doc
                              :header header)
                  result))))
      (nreverse result))))

;;; ---------------------------------------------------------------------------
;;; Exported-symbol discovery
;;; ---------------------------------------------------------------------------

(defun exported-symbols (lib-dir)
  "Every im* symbol exported by the built libraries, as a hash of name -> library.

Uses nm(1) on Unix. On a machine with no build, fall back to the .def files,
which are the Windows export lists and a good approximation."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (entry '(("libim"         . "lib-im")
                     ("libim_process" . "lib-im-process")
                     ("libim_fftw3"   . "lib-im-fftw3")
                     ("libim_jp2"     . "lib-im-jp2")
                     ("libim_heif"    . "lib-im-heif")
                     ("libim_capture" . "lib-im-capture")
                     ("libim_avi"     . "lib-im-avi")
                     ("libim_wmv"     . "lib-im-wmv")))
      (dolist (ext '(".dylib" ".so"))
        (let ((path (merge-pathnames (concatenate 'string (car entry) ext)
                                     (uiop:ensure-directory-pathname lib-dir))))
          (when (probe-file path)
            (dolist (line (uiop:split-string
                           (uiop:run-program (list "nm" "-gU" (namestring path))
                                             :output :string :ignore-error-status t)
                           :separator '(#\Newline)))
              (let* ((fields (remove "" (uiop:split-string (string-trim " " line)
                                                          :separator '(#\Space))
                                     :test #'string=))
                     (kind (second fields))
                     (sym (third fields)))
                ;; Only code symbols. nm also lists exported data -- libim_process
                ;; exports im_process_mincount, an int -- and a data symbol has no
                ;; signature to bind, so counting it as unbound would put a
                ;; permanent false entry in the coverage report.
                (when (and sym kind (string= kind "T") (> (length sym) 3))
                  ;; Mach-O prefixes every C symbol with an underscore.
                  (let ((clean (if (char= (char sym 0) #\_) (subseq sym 1) sym)))
                    (when (and (> (length clean) 2)
                               (string= "im" (subseq clean 0 2))
                               (not (gethash clean table)))
                      (setf (gethash clean table) (cdr entry)))))))))))
    table))

;;; ---------------------------------------------------------------------------
;;; Emission
;;; ---------------------------------------------------------------------------

(defparameter *internal-allowlist*
  '("imFormatRegisterTIFF" "imFormatRegisterJPEG" "imFormatRegisterPNG"
    "imFormatRegisterGIF" "imFormatRegisterBMP" "imFormatRegisterRAS"
    "imFormatRegisterLED" "imFormatRegisterSGI" "imFormatRegisterPCX"
    "imFormatRegisterTGA" "imFormatRegisterPNM" "imFormatRegisterPFM"
    "imFormatRegisterICO" "imFormatRegisterKRN"
    "imFormatInitRAW" "imFormatFinishRAW"
    "imFileCheckConversion" "imFileClear" "imFileSetBaseAttributes"
    "imFileLineBufferInit" "imFileLineBufferRead" "imFileLineBufferWrite"
    "imFileLineBufferInc" "imFileLineBufferCount" "imFileLineSizeAligned"
    "imFileFormatBaseNew" "imFileFormatBaseOpen" "imFileFormatBaseOpenAs"
    "imFormatRegister")
  "Exported symbols this binding deliberately does not bind.

All of them are for writing a format driver, not for using one: the fourteen
built-in imFormatRegister<FMT> entries are called by imFormatRegisterInternal
on IM's own behalf, and the imFile* internals belong to the driver base class.
They are also absent from src/im.def, so they do not exist on Windows at all --
binding them would produce a library that loads on Unix and fails on Windows.

The coverage report treats anything here as accounted for. Anything NOT here
and not bound is a gap, and the report is a test.")

(defun lisp-fn-name (c-name)
  (format nil "%im-~A" (kebab c-name :strip-im t)))

(defun render (object)
  "Print OBJECT the way a Lisp programmer would have typed it.

The default printer upcases symbol names, so :string comes out as :STRING and
the shipped source reads like machine output. Worse, ~A on a keyword drops the
colon entirely -- (file-name STRING) is not a CFFI type, it is a free variable
that happens to compile.

Non-keyword symbols print unqualified. They name CFFI types that will be
interned in IM.FFI when the generated file is read, but they exist here as
IM.GEN symbols, and PRIN1 would helpfully write im.gen::im-file."
  (etypecase object
    (keyword (string-downcase (prin1-to-string object)))
    ;; Enum type names arrive as strings ("data-type") and are already in the
    ;; spelling the output wants; PRIN1 would wrap them in quotes and turn a
    ;; type into a literal.
    (string object)
    (symbol (string-downcase (symbol-name object)))))

(defun render-type (type)
  (if (and (consp type) (eq :todo (car type)))
      ;; Keep the C spelling in a comment so the reviewer can see what was not
      ;; understood without going back to the header.
      (format nil ":pointer #| ~A |#" (string-trim " " (second type)))
      (render type)))

(defun c-integer (text)
  "Turn a C integer literal into a Lisp one.

0x100 is a symbol in Lisp, not a number, and #x100 is the number. Emitting the
C spelling produced a defcenum whose values were unbound variables."
  (let ((s (string-trim " " text)))
    (cond ((and (> (length s) 2) (string-equal "0x" (subseq s 0 2)))
           (format nil "#x~A" (subseq s 2)))
          (t s))))

(defun lisp-string (text)
  "TEXT as a Lisp string literal, with quotes and backslashes escaped.

IM's own documentation quotes things -- the imFileOpen comment says the Lua
metatable is named \"imFile\" -- and an unescaped quote closes the docstring
early, leaving the rest of the sentence to be read as code."
  (with-output-to-string (s)
    (write-char #\" s)
    (loop for c across text
          do (when (or (char= c #\") (char= c #\\)) (write-char #\\ s))
             (write-char c s))
    (write-char #\" s)))

(defun escape-string-body (text)
  "TEXT with quotes and backslashes escaped, but no surrounding quotes."
  (with-output-to-string (s)
    (loop for c across text
          do (when (or (char= c #\") (char= c #\\)) (write-char #\\ s))
             (write-char c s))))

(defun wrap-docstring (doc indent)
  "DOC as a wrapped Lisp string literal, opening quote indented by INDENT.

Continuation lines sit at column 0, which is how docstrings are conventionally
written in this codebase and everywhere else -- indenting them would put the
leading spaces inside the string."
  (when doc
    (let ((words (remove "" (uiop:split-string (escape-string-body doc)
                                               :separator '(#\Space))
                         :test #'string=))
          (lines '())
          (current ""))
      (dolist (w words)
        (cond ((string= current "") (setf current w))
              ((> (+ (length current) 1 (length w)) 74)
               (push current lines)
               (setf current w))
              (t (setf current (concatenate 'string current " " w)))))
      (when (plusp (length current)) (push current lines))
      (setf lines (nreverse lines))
      (when lines
        (format nil "~A\"~{~A~^~%~}\""
                (make-string indent :initial-element #\Space)
                lines)))))

(defun power-of-two-p (n)
  (and (integerp n) (plusp n) (zerop (logand n (1- n)))))

(defun bitfield-p (members)
  "True when MEMBERS look like flags rather than a sequence.

IM writes both as `enum`, but they are not the same thing: imColorModeConfig's
IM_ALPHA, IM_PACKED and IM_TOPDOWN are 0x100, 0x200 and 0x400 and get OR'd
together into the same int as the colour space, while imDataType's members are
0..7 and only ever appear one at a time. CFFI needs to be told which is which
-- FOREIGN-BITFIELD-SYMBOLS on a DEFCENUM signals \"is not a foreign bitfield
type\", and DEFCENUM on flags cannot represent a combination at all.

The test: every member carries an explicit value, and every value is a
distinct power of two. That accepts imColorModeConfig and imToneGamutFlags,
and rejects imGammaFactor (0, -10, -1000, 2, 7) and imBinFileModule (0..5)."
  (and members
       (every #'cdr members)
       (let ((values (mapcar (lambda (m)
                               (let ((text (c-integer (cdr m))))
                                 (ignore-errors
                                  (if (and (> (length text) 2)
                                           (string= "#x" (subseq text 0 2)))
                                      (parse-integer text :start 2 :radix 16)
                                      (parse-integer text)))))
                             members)))
         (and (every #'power-of-two-p values)
              (= (length values) (length (remove-duplicates values)))))))

(defun emit-enum (stream enum)
  (destructuring-bind (c-name lisp-name members) enum
    (let ((prefix (common-member-prefix (mapcar #'car members)))
          (bitfield (bitfield-p members)))
      (format stream "~%;;; ~A~%(cffi:~A ~A"
              c-name
              (if bitfield "defbitfield" "defcenum")
              lisp-name)
      (dolist (m members)
        (let ((kw (render (enum-keyword lisp-name (car m) prefix))))
          (if (cdr m)
              (format stream "~%  (~A ~A)" kw (c-integer (cdr m)))
              (format stream "~%  ~A" kw))))
      (format stream ")~%"))))

(defun emit-defcfun (stream decl)
  (let ((todo (or (and (consp (decl-return decl)) (eq :todo (car (decl-return decl))))
                  (some (lambda (p) (and (consp (cdr p)) (eq :todo (car (cdr p)))))
                        (decl-params decl)))))
    (when todo
      (format stream "~%;; REVIEW: unmapped C type(s) below; check against ~A~%"
              (decl-header decl)))
    (format stream "~%(cffi:defcfun (\"~A\" ~A) ~A"
            (decl-name decl) (lisp-fn-name (decl-name decl))
            (render-type (decl-return decl)))
    (let ((doc (wrap-docstring (decl-doc decl) 2)))
      (when doc (format stream "~%~A" doc)))
    (dolist (p (decl-params decl))
      (format stream "~%  (~A ~A)" (car p) (render-type (cdr p))))
    (format stream ")~%")))

(defparameter *header-groups*
  '(("im-lib"          "im_lib.h")
    ("im-file"         "im.h" "im_raw.h")
    ("im-image"        "im_image.h")
    ("im-palette"      "im_palette.h")
    ("im-convert"      "im_convert.h")
    ("im-counter"      "im_counter.h")
    ("im-util"         "im_util.h")
    ("im-binfile"      "im_binfile.h")
    ("im-kernel"       "im_kernel.h")
    ("im-attrib"       "im_attrib_flat.h")
    ("im-color"        "im_color.h" "im_colorhsi.h")
    ("im-old"          "im_old.h")
    ("im-capture"      "im_capture.h")
    ("im-format-addon" "im_format_jp2.h" "im_format_heif.h"
                       "im_format_avi.h" "im_format_wmv.h")
    ("im-process-pnt"  "im_process_pnt.h")
    ("im-process-loc"  "im_process_loc.h")
    ("im-process-glo"  "im_process_glo.h")
    ("im-process-ana"  "im_process_ana.h"))
  "Output file <- upstream headers. One file per coherent area, not strictly
one per header: im.h and im_raw.h are both the file API, and the two colour
headers belong together.")

(defun generate (source-root &key (output "src/ffi/") (lib-dir nil))
  "Draft src/ffi/*.lisp from the headers under SOURCE-ROOT/include/.

LIB-DIR defaults to SOURCE-ROOT/build/lib/ and is used only to decide which
declarations correspond to symbols that actually exist."
  (let* ((root (uiop:ensure-directory-pathname source-root))
         (include (merge-pathnames "include/" root))
         (libs (uiop:ensure-directory-pathname
                (or lib-dir (merge-pathnames "build/lib/" root))))
         (exports (exported-symbols libs))
         (out (uiop:ensure-directory-pathname output))
         (bound (make-hash-table :test #'equal))
         (all-enums '()))
    (ensure-directories-exist out)
    (format t "~&Exported im* symbols found: ~D~%" (hash-table-count exports))

    ;; types.lisp must be read before anything that mentions these names.
    (with-open-file (s (merge-pathnames "types.lisp" out)
                       :direction :output :if-exists :supersede)
      (format s ";;;; src/ffi/types.lisp — DRAFTED by tools/gen-bindings.lisp.~%")
      (format s ";;;;~%;;;; IM's opaque handles. Each is a :POINTER underneath, but naming them~%")
      (format s ";;;; makes a signature say which kind of pointer it wants: imFileClose takes~%")
      (format s ";;;; an im-file and imImageDestroy an im-image, and mixing them up is then~%")
      (format s ";;;; visible in the source rather than at runtime.~%~%")
      (format s "(in-package #:im.ffi)~%~%")
      (dolist (entry *opaque-types*)
        (format s "(cffi:defctype ~A :pointer)   ; ~A*~%"
                (render (cdr entry)) (car entry))))

    ;; Enums first: map-type consults *enum-types*, so a parameter declared
    ;; `imDataType data_type` becomes the enum rather than an int.
    (dolist (group *header-groups*)
      (dolist (h (rest group))
        (let ((path (merge-pathnames h include)))
          (when (probe-file path)
            (dolist (e (parse-enums (read-header path)))
              (setf (gethash (first e) *enum-types*) (second e))
              (push (cons h e) all-enums))))))

    (dolist (group *header-groups*)
      (destructuring-bind (file &rest headers) group
        (let ((decls '()) (enums '()))
          (dolist (h headers)
            (let ((path (merge-pathnames h include)))
              (when (probe-file path)
                (let ((text (read-header path)))
                  (setf enums (append enums
                                      (mapcar #'cdr (remove h all-enums
                                                            :key #'car
                                                            :test-not #'string=))))
                  (dolist (d (parse-decls text h))
                    ;; The heart of it: bind only what a library exports.
                    (when (and (gethash (decl-name d) exports)
                               (not (member (decl-name d) *internal-allowlist*
                                            :test #'string=))
                               (not (gethash (decl-name d) bound)))
                      (setf (gethash (decl-name d) bound) t)
                      (push d decls)))))))
          (setf decls (nreverse decls))
          (when (or decls enums)
            (with-open-file (s (merge-pathnames (format nil "~A.lisp" file) out)
                               :direction :output :if-exists :supersede)
              (format s ";;;; src/ffi/~A.lisp — DRAFTED by tools/gen-bindings.lisp.~%" file)
              (format s ";;;;~%;;;; Source: ~{~A~^, ~}~%" headers)
              (format s ";;;; Hand corrections below this line are expected and are kept;~%")
              (format s ";;;; re-run the generator into a clean tree and diff.~%~%")
              (format s "(in-package #:im.ffi)~%")
              (dolist (e enums) (emit-enum s e))
              (dolist (d decls) (emit-defcfun s d)))
            (format t "~&  ~A.lisp: ~D function~:P, ~D enum~:P~%"
                    file (length decls) (length enums))))))

    ;; A manifest of every C name bound, so the test suite can check at
    ;; RUNTIME that each one resolves in the loaded libraries. The generator's
    ;; own coverage check only runs when someone regenerates; this one runs on
    ;; every CI job, against whatever IM that machine actually has, and is what
    ;; catches a binding to a function upstream has removed.
    (with-open-file (s (merge-pathnames "manifest.lisp" out)
                       :direction :output :if-exists :supersede)
      (format s ";;;; src/ffi/manifest.lisp — DRAFTED by tools/gen-bindings.lisp.~%")
      (format s ";;;;~%;;;; Every C function this binding declares, and the library it was~%")
      (format s ";;;; found in when the bindings were generated. Used by the test suite.~%~%")
      (format s "(in-package #:im.ffi)~%~%")
      (format s "(defparameter *bindings*~%  '(")
      (let ((first t))
        (maphash (lambda (sym lib)
                   (when (gethash sym bound)
                     (format s "~:[~%    ~;~](\"~A\" . \"~A\")" first sym lib)
                     (setf first nil)))
                 exports))
      (format s "))~%"))

    (coverage-report exports bound)))

(defun coverage-report (exports bound)
  "Print, and return, the symbols exported but not bound.

This is the check that the previous binding lacked. Wired into the test suite,
it fails the build when upstream adds a function nobody bound, and when a
bound function stops existing."
  (let (missing)
    (maphash (lambda (sym lib)
               (unless (or (gethash sym bound)
                           (member sym *internal-allowlist* :test #'string=))
                 (push (cons sym lib) missing)))
             exports)
    (setf missing (sort missing #'string< :key #'car))
    (format t "~&~%Coverage: ~D bound, ~D exported, ~D unbound~%"
            (hash-table-count bound) (hash-table-count exports) (length missing))
    (dolist (m missing) (format t "  UNBOUND ~A (~A)~%" (car m) (cdr m)))
    missing))
