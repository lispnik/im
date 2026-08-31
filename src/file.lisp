;;;; src/file.lisp — reading and writing image files.

(in-package #:im)

(export '(load
          save
          file-info
          frame-count
          format-list
          format-info
          format-compressions
          format-can-write-p
          attributes
          attribute
          image-attributes
          image-attribute
          set-image-attribute
          image-attribute-string
          set-attribute-string
          remove-attribute
          with-temporary-attribute
          guess-format))

(defun %namestring (pathname)
  "IM takes a C string; a logical pathname would reach it untranslated."
  (namestring (translate-logical-pathname pathname)))

;;; Whole-file operations -----------------------------------------------------
;;;
;;; imFileImageLoad and imFileImageSave open, transfer and close in one call.
;;; The previous binding exposed the three-step form as the primary API, which
;;; meant every caller wrote the same UNWIND-PROTECT around a file handle.

(defun load (pathname &key (index 0))
  "Read the image at INDEX from PATHNAME and return an IMAGE.

INDEX selects a frame in a multi-image file -- a GIF animation, a TIFF stack,
a volume -- and is 0 for the single-image case. Attributes stored in the file
come back on the image; see ATTRIBUTES.

The image owns foreign memory. Prefer WITH-IMAGE, which releases it on unwind."
  (cffi:with-foreign-object (err 'im.ffi::error-code)
    (let ((pointer (im.ffi::%im-file-image-load (%namestring pathname) index err)))
      ;; Check the error code before the pointer: IM reports why it failed
      ;; there, and a NULL with :ERROR-CODE-NONE would be a bug worth seeing
      ;; as a memory error rather than as a mysterious NIL.
      (maybe-error (cffi:mem-ref err 'im.ffi::error-code) pathname)
      (wrap-handle pointer))))

(defun save (image pathname &key format compression)
  "Write IMAGE to PATHNAME. Returns IMAGE.

FORMAT is IM's name for the file format -- \"PNG\", \"TIFF\", \"JPEG\" -- and
defaults to a guess from the filename extension. COMPRESSION is format
specific; FORMAT-COMPRESSIONS lists what a given format accepts, and NIL takes
the format's default."
  (let* ((path (%namestring pathname))
         (format (or format (guess-format pathname))))
    (if (null compression)
        ;; No compression requested: one call, and the format picks its own.
        (maybe-error
         (cffi:foreign-enum-keyword
          'im.ffi::error-code
          (im.ffi::%im-file-image-save path format (handle image)))
         pathname)
        ;; Compression requested: imFileImageSave cannot express it. Setting a
        ;; "Compression" attribute on the image does NOT work -- it is quietly
        ;; ignored, and the file comes back with the format's default. That is
        ;; easy to miss because the default for TIFF happens to be LZW, so
        ;; asking for LZW "worked" while asking for NONE produced LZW too.
        ;;
        ;; Compression belongs to the file, and imFileSetInfo is how it is set,
        ;; between creating the file and writing to it.
        (let ((file (cffi:with-foreign-object (err 'im.ffi::error-code)
                      (let ((handle (im.ffi::%im-file-new path format err)))
                        (maybe-error (cffi:mem-ref err 'im.ffi::error-code) pathname)
                        handle))))
          (unwind-protect
               (progn
                 (im.ffi::%im-file-set-info file compression)
                 (maybe-error
                  (cffi:foreign-enum-keyword
                   'im.ffi::error-code
                   (im.ffi::%im-file-save-image file (handle image)))
                  pathname))
            (im.ffi::%im-file-close file))))
    image))

(defparameter *extension-formats*
  '(("jpg" . "JPEG") ("jpeg" . "JPEG") ("jpe" . "JPEG")
    ("png" . "PNG")
    ("tif" . "TIFF") ("tiff" . "TIFF")
    ("gif" . "GIF")
    ("bmp" . "BMP")
    ("ras" . "RAS") ("sun" . "RAS")
    ("ico" . "ICO")
    ("pnm" . "PNM") ("ppm" . "PNM") ("pgm" . "PNM") ("pbm" . "PNM")
    ("pfm" . "PFM")
    ("krn" . "KRN")
    ("led" . "LED")
    ("sgi" . "SGI") ("rgb" . "SGI") ("bw" . "SGI")
    ("pcx" . "PCX")
    ("tga" . "TGA") ("tpic" . "TGA")
    ("jp2" . "JP2")
    ("heic" . "HEIF") ("heif" . "HEIF")
    ("avif" . "AVIF"))
  "Filename extension to IM format name.

IM itself identifies formats by content when reading and requires an explicit
name when writing, so this table exists only to make SAVE's FORMAT argument
optional. Taken from the per-format documentation in im_format_all.h.")

(defun guess-format (pathname)
  "The IM format name implied by PATHNAME's type, or signal FORMAT-ERROR."
  (let* ((type (pathname-type pathname))
         (name (and type (cdr (assoc (string-downcase type) *extension-formats*
                                     :test #'string=)))))
    (or name
        (cl:error 'format-error
                  :detail (format nil "~A (no format known for extension ~S; ~
                                       pass :format explicitly)"
                                  pathname type)))))

;;; File inspection -----------------------------------------------------------

(defmacro with-open-image-file ((var pathname) &body body)
  "Open PATHNAME for reading, bind VAR to the imFile*, and close on unwind."
  (alexandria:with-gensyms (path err)
    `(let* ((,path (%namestring ,pathname))
            (,var (cffi:with-foreign-object (,err 'im.ffi::error-code)
                    (let ((handle (im.ffi::%im-file-open ,path ,err)))
                      (maybe-error (cffi:mem-ref ,err 'im.ffi::error-code) ,pathname)
                      handle))))
       (unwind-protect (progn ,@body)
         (im.ffi::%im-file-close ,var)))))

(defun file-info (pathname)
  "Everything IM knows about PATHNAME's header, as a property list.

Returns :PATHNAME, :FORMAT, :COMPRESSION, :FRAME-COUNT and :FRAMES, where each
frame is a plist of :WIDTH, :HEIGHT, :COLOR-SPACE, :COLOR-MODE-CONFIG,
:DATA-TYPE, :DATA-SIZE and :ATTRIBUTES.

Reads headers only -- no pixel data is transferred, so this is cheap on a
large file."
  (with-open-image-file (file pathname)
    (cffi:with-foreign-objects ((format :char 64)
                                (compression :char 64)
                                (frame-count :int))
      ;; The buffers are IM's documented sizes. imFileGetInfo does a plain
      ;; strcpy into them, so undersizing is a stack smash, not a truncation.
      (im.ffi::%im-file-get-info file format compression frame-count)
      (let ((count (cffi:mem-ref frame-count :int)))
        (list :pathname pathname
              :format (cffi:foreign-string-to-lisp format)
              :compression (cffi:foreign-string-to-lisp compression)
              :frame-count count
              :frames (loop for index below count
                            collect (%frame-info file index)))))))

(defun %frame-info (file index)
  (cffi:with-foreign-objects ((width :int) (height :int)
                              (color-mode :int) (data-type :int))
    (maybe-error
     (cffi:foreign-enum-keyword
      'im.ffi::error-code
      (im.ffi::%im-file-read-image-info file index width height color-mode data-type))
     (format nil "frame ~D" index))
    (let ((mode (cffi:mem-ref color-mode :int))
          (w (cffi:mem-ref width :int))
          (h (cffi:mem-ref height :int))
          (dt (cffi:mem-ref data-type :int)))
      (list :width w
            :height h
            :color-space (cffi:foreign-enum-keyword 'im.ffi::color-space
                                                    (logand mode #xff))
            :color-mode-config (cffi:foreign-bitfield-symbols
                                'im.ffi::color-mode-config (logand mode #xff00))
            :data-type (cffi:foreign-enum-keyword 'im.ffi::data-type dt)
            :data-size (im.ffi::%im-image-data-size w h mode dt)
            :attributes (%file-attributes file)))))

(defun frame-count (pathname)
  "Number of images in PATHNAME: animation frames, stack slices, volume depth."
  (getf (file-info pathname) :frame-count))

;;; Attributes ----------------------------------------------------------------
;;;
;;; Two things carry attributes -- an open imFile and an imImage -- and they
;;; carry them identically: a name, an imDataType, a count, and a block of that
;;; many values. Only the three FFI entry points differ, so they are passed in
;;; as functions rather than having the decoding written out twice.
;;;
;;; The file side here is read-only. An attribute reaches a file by being set
;;; on the image that SAVE writes, which is why the setters below take an
;;; IMAGE and there is no file-level counterpart.

(defparameter *attribute-c-types*
  '((:data-type-byte    . :unsigned-char)
    (:data-type-short   . :short)
    (:data-type-ushort  . :unsigned-short)
    (:data-type-int     . :int)
    (:data-type-float   . :float)
    (:data-type-double  . :double)
    (:data-type-cfloat  . :float)
    (:data-type-cdouble . :double))
  "The C type one value of each imDataType is stored as.

The two complex types share a storage type with their real counterparts and
take two cells per value: IM stores a complex as adjacent real and imaginary
parts, not as a struct.")

(defparameter *attribute-integer-limits*
  '((:unsigned-char  0 . 255)
    (:short     -32768 . 32767)
    (:unsigned-short 0 . 65535)
    (:int  -2147483648 . 2147483647))
  "What fits in each integral attribute cell.

Checked before the write rather than left to CFFI, so that storing 300 in a
byte attribute is an error naming the value instead of a wrapped 44.")

(defun %attribute-c-type (data-type)
  (or (cdr (assoc data-type *attribute-c-types*))
      (cl:error 'data-error
                :detail (format nil "attribute data type ~S" data-type))))

(defun %complex-data-type-p (data-type)
  (member data-type '(:data-type-cfloat :data-type-cdouble)))

;;; Reading -------------------------------------------------------------------

(defun %attribute-names (list-fn)
  "The attribute names LIST-FN lists, as fresh Lisp strings.

LIST-FN takes a names array and a count, IM's two-call protocol: a null array
asks for the count, and a second call fills the array in.

The names IM hands back point into its own attribute table, and the header
warns they are invalidated by the next set or remove -- so they are copied
here, before anything else can run."
  (cffi:with-foreign-object (count :int)
    (funcall list-fn (cffi:null-pointer) count)
    (let ((n (cffi:mem-ref count :int)))
      (when (plusp n)
        (cffi:with-foreign-object (names :pointer n)
          (setf (cffi:mem-ref count :int) n)
          (funcall list-fn names count)
          (loop for i below (cffi:mem-ref count :int)
                collect (cffi:foreign-string-to-lisp
                         (cffi:mem-aref names :pointer i))))))))

(defun %attribute-value (get-fn name)
  "NAME's value as (VALUE DATA-TYPE COUNT), or NIL when there is no such
attribute. GET-FN takes a name and the two out-parameters IM fills in."
  (cffi:with-foreign-objects ((data-type :int) (count :int))
    (let ((pointer (funcall get-fn name data-type count)))
      (unless (cffi:null-pointer-p pointer)
        (let* ((type (cffi:foreign-enum-keyword 'im.ffi::data-type
                                                (cffi:mem-ref data-type :int)))
               (n (cffi:mem-ref count :int)))
          (list (%decode-attribute pointer type n) type n))))))

(defun %decode-attribute (pointer type count)
  "COUNT values of TYPE at POINTER, as a Lisp value.

Byte attributes are returned as a string when they look like one. IM stores
text this way -- \"Author\", \"Software\", the EXIF strings -- and the previous
binding returned a vector of character codes and told the caller to run it
through Babel themselves.

The complex types decode to CL:COMPLEX numbers. They used to fall through to
the byte branch, which reported the bytes of a float as small integers:
plausible-looking, and wrong."
  (let ((c-type (%attribute-c-type type)))
    (flet ((reals ()
             (let ((v (make-array count)))
               (dotimes (i count v)
                 (setf (aref v i) (cffi:mem-aref pointer c-type i)))))
           (complexes ()
             (let ((v (make-array count)))
               (dotimes (i count v)
                 (setf (aref v i)
                       (complex (cffi:mem-aref pointer c-type (* 2 i))
                                (cffi:mem-aref pointer c-type (1+ (* 2 i)))))))))
      (cond ((%complex-data-type-p type) (complexes))
            ((eq type :data-type-byte)
             (let ((bytes (reals)))
               (if (%printable-bytes-p bytes)
                   ;; Trailing NUL is a C string terminator, not content.
                   (map 'string #'code-char
                        (remove 0 bytes :start (max 0 (1- (length bytes)))))
                   bytes)))
            (t (reals))))))

(defun %printable-bytes-p (bytes)
  "True when BYTES look like text: printable ASCII, with at most a final NUL."
  (and (plusp (length bytes))
       (every (lambda (b) (or (<= 32 b 126) (member b '(0 9 10 13))))
              bytes)
       ;; A single NUL is a terminator; NULs in the middle mean binary.
       (let ((nul (position 0 bytes)))
         (or (null nul) (= nul (1- (length bytes)))))))

;;; Writing -------------------------------------------------------------------

(defun %attribute-values (name value)
  "VALUE as a vector of numbers, whatever shape the caller passed it in."
  (typecase value
    (number (vector value))
    (list (coerce value 'vector))
    ((and vector (not string)) value)
    (t (cl:error 'data-error
                 :detail (format nil "attribute ~A: ~S is not a number or a ~
                                      sequence of numbers"
                                 name value)))))

(defun %infer-data-type (name values)
  "The narrowest imDataType that stores every element of VALUES.

Exactly, for everything IM has a type for. A ratio is the exception it cannot
have one for -- 1/3 becomes a double and is no longer 1/3 -- so it is named
here rather than left for the caller to notice in a file."
  (cond ((every #'integerp values)
         (if (every (lambda (v) (typep v '(signed-byte 32))) values)
             :data-type-int
             ;; No integral imDataType is wider than 32 bits. Storing such a
             ;; value as a double would round it; saying so is better.
             (cl:error 'data-error
                       :detail (format nil "attribute ~A: no IM integer type ~
                                            holds ~S -- it needs more than 32 bits"
                                       name (find-if-not
                                             (lambda (v) (typep v '(signed-byte 32)))
                                             values)))))
        ((every (lambda (v) (and (realp v) (not (and (rationalp v) (not (integerp v))))))
                values)
         :data-type-double)
        ((every #'realp values)
         (cl:error 'data-error
                   :detail (format nil "attribute ~A: no IM type holds ~S ~
                                        exactly -- pass a float, or ~
                                        :data-type :data-type-double to round it"
                                   name (find-if (lambda (v)
                                                   (and (rationalp v)
                                                        (not (integerp v))))
                                                 values))))
        ((every #'numberp values) :data-type-cdouble)
        (t (cl:error 'data-error
                     :detail (format nil "attribute ~A: ~S is not a number"
                                     name (find-if-not #'numberp values))))))

(defun %attribute-cell (name c-type value)
  "VALUE as something CFFI can store in one C-TYPE cell.

Every rejection here is a DATA-ERROR. The float branches used to hand the
value straight to COERCE, which reports a complex as a CL:TYPE-ERROR and an
out-of-range magnitude as a FLOATING-POINT-OVERFLOW -- both real errors, and
both outside the IM-ERROR hierarchy a caller wraps attribute writes in."
  (let ((limits (cdr (assoc c-type *attribute-integer-limits*))))
    (cond (limits
           (unless (and (integerp value) (<= (car limits) value (cdr limits)))
             (cl:error 'data-error
                       :detail (format nil "attribute ~A: ~S does not fit a ~
                                            ~(~A~) cell (~D..~D)"
                                       name value c-type
                                       (car limits) (cdr limits))))
           value)
          ((eq c-type :float) (%float-attribute-cell name value 'single-float))
          (t (%float-attribute-cell name value 'double-float)))))

(defun %float-attribute-cell (name value type)
  (unless (realp value)
    (cl:error 'data-error
              :detail (format nil "attribute ~A: ~S is not a real number, and ~
                                   a ~(~A~) cell holds one at a time -- ask ~
                                   for a complex data type to store both parts"
                              name value type)))
  ;; Range-checked rather than trapped. COERCE signals FLOATING-POINT-OVERFLOW
  ;; on the platforms where SBCL enables the traps and quietly returns an
  ;; infinity on the ones where it does not -- arm64 Linux among them, which is
  ;; where CI caught this after the trap-based version passed everywhere else.
  ;; An infinity in an attribute is not a value any format can write back.
  (let ((limit (ecase type
                 (single-float most-positive-single-float)
                 (double-float most-positive-double-float))))
    (unless (<= (abs value) limit)
      (cl:error 'data-error
                :detail (format nil "attribute ~A: ~S is out of range for ~(~A~)"
                                name value type))))
  (handler-case (coerce value type)
    (arithmetic-error ()
      (cl:error 'data-error
                :detail (format nil "attribute ~A: ~S is out of range for ~(~A~)"
                                name value type)))))

(defun %encode-attribute (name pointer type values)
  "Write VALUES into POINTER as cells of TYPE."
  (let ((c-type (%attribute-c-type type)))
    (if (%complex-data-type-p type)
        ;; Real and imaginary parts alternate, so value i occupies cells 2i
        ;; and 2i+1. REALPART of a real is itself, so this also accepts a
        ;; caller who asked for a complex type and passed plain numbers.
        (dotimes (i (length values))
          (let ((v (aref values i)))
            (setf (cffi:mem-aref pointer c-type (* 2 i))
                  (%attribute-cell name c-type (realpart v))
                  (cffi:mem-aref pointer c-type (1+ (* 2 i)))
                  (%attribute-cell name c-type (imagpart v)))))
        (dotimes (i (length values))
          (setf (cffi:mem-aref pointer c-type i)
                (%attribute-cell name c-type (aref values i)))))))

;;; Image attributes ----------------------------------------------------------

(defun image-attributes (image)
  "Every attribute on IMAGE, as an alist of (NAME VALUE DATA-TYPE COUNT).

The shape ATTRIBUTES returns for a file, and the way to see what a loaded
image is carrying before SAVE writes it back out."
  (let ((handle (handle image)))
    (loop for name in (%attribute-names
                       (lambda (names count)
                         (im.ffi::%im-image-get-attribute-list handle names count)))
          collect (cons name (%attribute-value
                              (lambda (name type count)
                                (im.ffi::%im-image-get-attribute handle name type count))
                              name)))))

(defun image-attribute (image name)
  "IMAGE's NAME attribute, as three values: value, data type and count.

NIL when the image has no attribute called NAME. ATTRIBUTE, which answers the
same question about a file on disk, returns the three as a list instead --
there it is the tail of an alist entry."
  (let* ((handle (handle image))
         (entry (%attribute-value
                 (lambda (name type count)
                   (im.ffi::%im-image-get-attribute handle name type count))
                 name)))
    (when entry (values-list entry))))

(defun set-image-attribute (image name value &key data-type)
  "Attach VALUE to IMAGE as attribute NAME. Returns IMAGE.

VALUE is a string, a number, or a sequence of numbers. DATA-TYPE names the
imDataType to store it as -- :DATA-TYPE-BYTE, -SHORT, -USHORT, -INT, -FLOAT,
-DOUBLE, -CFLOAT or -CDOUBLE -- and defaults to the narrowest type that holds
VALUE without rounding it: byte for a string, int for integers, double for
other reals, cdouble for complex numbers. A value that does not fit the type
asked for is an error, not a truncation.

A NIL VALUE removes the attribute, so (SETF (IMAGE-ATTRIBUTE ...) NIL) means
what it looks like.

What survives SAVE is the format's business: each one stores the attributes it
understands and drops the rest, and IM checks nothing here. A misspelled name
is written to the image and quietly missing from the file, so confirm with
ATTRIBUTES on the file rather than assuming.

Byte attributes do not round-trip as vectors. IM stores text in them and the
reader hands text back, so #(65 66) written here reads as \"AB\" -- write a
string when it is one, and expect one back for any byte vector that happens to
look like printable ASCII."
  (cond
    ((null value)
     (remove-attribute image name))
    ((stringp value)
     (let ((type (or data-type :data-type-byte)))
       (unless (eq type :data-type-byte)
         (cl:error 'data-error
                   :detail (format nil "attribute ~A: a string is IM's byte ~
                                        type; ~S cannot hold one"
                                   name type)))
       ;; imImageSetAttribString is imImageSetAttribute with count -1 and the
       ;; zero terminator taken care of.
       (im.ffi::%im-image-set-attrib-string (handle image) name value)))
    (t
     (let* ((values (%attribute-values name value))
            (type (or data-type (%infer-data-type name values)))
            (count (length values)))
       (when (zerop count)
         (cl:error 'data-error
                   :detail (format nil "attribute ~A has no values; pass NIL ~
                                        to remove it"
                                   name)))
       (cffi:with-foreign-object (data (%attribute-c-type type)
                                       (if (%complex-data-type-p type)
                                           (* 2 count)
                                           count))
         (%encode-attribute name data type values)
         (im.ffi::%im-image-set-attribute
          (handle image) name
          (cffi:foreign-enum-value 'im.ffi::data-type type)
          count data)))))
  image)

(defun (setf image-attribute) (value image name)
  (set-image-attribute image name value)
  value)

(defun image-attribute-string (image name)
  "The string value of IMAGE's NAME attribute, or NIL if it has none.

The fast path for the common case; IMAGE-ATTRIBUTE answers for every type."
  ;; The binding declares a :STRING return, so CFFI hands back NIL for NULL and
  ;; a fresh Lisp string otherwise. IM owns the C storage; nothing to free.
  (im.ffi::%im-image-get-attrib-string (handle image) name))

(defun set-attribute-string (image name value)
  "Attach a string attribute to IMAGE."
  (im.ffi::%im-image-set-attrib-string (handle image) name value)
  image)

(defun remove-attribute (image name)
  "Remove attribute NAME from IMAGE.

IM spells removal as a set with a NULL value and zero count."
  (im.ffi::%im-image-set-attribute
   (handle image) name
   (cffi:foreign-enum-value 'im.ffi::data-type :data-type-byte)
   0 (cffi:null-pointer))
  image)

(defmacro with-temporary-attribute ((image name value) &body body)
  "Set string attribute NAME on IMAGE for the extent of BODY, then restore it.

A NIL VALUE leaves the attribute alone entirely, so callers need not
special-case the common path."
  (alexandria:with-gensyms (img attr val previous)
    `(let* ((,img ,image)
            (,attr ,name)
            (,val ,value)
            (,previous (when ,val (image-attribute-string ,img ,attr))))
       (when ,val (set-attribute-string ,img ,attr ,val))
       (unwind-protect (progn ,@body)
         (when ,val
           (if ,previous
               (set-attribute-string ,img ,attr ,previous)
               (remove-attribute ,img ,attr)))))))

;;; File attributes -----------------------------------------------------------

(defun %file-attributes (file)
  "Every attribute on FILE as an alist of (NAME VALUE DATA-TYPE COUNT)."
  (loop for name in (%attribute-names
                     (lambda (names count)
                       (im.ffi::%im-file-get-attribute-list file names count)))
        collect (cons name (%attribute-value
                            (lambda (name type count)
                              (im.ffi::%im-file-get-attribute file name type count))
                            name))))

(defun attributes (pathname)
  "Every attribute of PATHNAME's first frame, as an alist."
  (with-open-image-file (file pathname)
    (cffi:with-foreign-objects ((w :int) (h :int) (cm :int) (dt :int))
      ;; Attributes are only populated after the header has been read, and the
      ;; read can fail -- a truncated or unsupported frame. Discarding the code
      ;; here (as this did) reported an empty alist, which reads as "this file
      ;; has no attributes" rather than "this file could not be parsed".
      (maybe-error
       (cffi:foreign-enum-keyword
        'im.ffi::error-code
        (im.ffi::%im-file-read-image-info file 0 w h cm dt))
       pathname))
    (%file-attributes file)))

(defun attribute (pathname name)
  "One named attribute of PATHNAME's first frame, as (VALUE DATA-TYPE COUNT).

NIL when the file has no attribute called NAME."
  (cdr (assoc name (attributes pathname) :test #'string=)))

;;; Format registry -----------------------------------------------------------

(defun format-list ()
  "The IM format names currently registered, e.g. (\"TIFF\" \"JPEG\" ...).

Reflects which add-ons loaded, so JP2 and HEIF appear only when their
libraries were found."
  (cffi:with-foreign-objects ((names :pointer 64) (count :int))
    (setf (cffi:mem-ref count :int) 64)
    (im.ffi::%im-format-list names count)
    (loop for i below (cffi:mem-ref count :int)
          collect (cffi:foreign-string-to-lisp (cffi:mem-aref names :pointer i)))))

(defun format-info (format)
  "(DESCRIPTION EXTENSIONS CAN-SEQUENCE-P) for a registered FORMAT."
  (cffi:with-foreign-objects ((desc :char 128) (ext :char 128) (seq :int))
    (maybe-error
     (cffi:foreign-enum-keyword
      'im.ffi::error-code (im.ffi::%im-format-info format desc ext seq))
     format)
    (list (cffi:foreign-string-to-lisp desc)
          (cffi:foreign-string-to-lisp ext)
          (not (zerop (cffi:mem-ref seq :int))))))

(defun format-compressions (format &key (color-mode -1) (data-type -1))
  "The compression names FORMAT accepts.

COLOR-MODE and DATA-TYPE narrow the answer to what is available for a
particular kind of image. Both default to -1, which is IM's \"ignore this\"
value and yields the format's complete list.

The obvious-looking default of 0 is wrong and quietly so: 0 is not a neutral
value, it is IM_RGB and IM_BYTE. Defaulting to it returned 7 of TIFF's 15
compressions -- CCITTFAX3, CCITTFAX4, SGILOG and THUNDERSCAN are all absent
for RGB byte data -- so a caller listing the options to choose one would
conclude a supported compression did not exist."
  (cffi:with-foreign-objects ((names :pointer 64) (count :int))
    (setf (cffi:mem-ref count :int) 64)
    (maybe-error
     (cffi:foreign-enum-keyword
      'im.ffi::error-code
      (im.ffi::%im-format-compressions format names count color-mode data-type))
     format)
    (loop for i below (cffi:mem-ref count :int)
          collect (cffi:foreign-string-to-lisp (cffi:mem-aref names :pointer i)))))

(defun format-can-write-p (format compression color-mode data-type)
  "True when FORMAT can store this kind of image at this COMPRESSION.

COMPRESSION is a name from FORMAT-COMPRESSIONS. COLOR-MODE and DATA-TYPE are
the integer encodings, not keywords -- use CFFI:FOREIGN-ENUM-VALUE, or take
them from FILE-INFO.

Unlike FORMAT-COMPRESSIONS, this does NOT accept -1 as a wildcard: it answers
about one specific combination, and -1 is simply a combination no format
supports. Passing it reports NIL for everything.

imFormatCanWriteImage returns an error CODE, not a boolean: IM_ERR_NONE, which
is zero, is the affirmative answer. Testing it for non-zero the way its name
invites gives exactly the wrong result for every input."
  (eq :error-code-none
      (cffi:foreign-enum-keyword
       'im.ffi::error-code
       (im.ffi::%im-format-can-write-image format compression color-mode data-type))))
