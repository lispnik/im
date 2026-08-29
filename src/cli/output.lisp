;;;; src/cli/output.lisp — human-readable and JSON output.
;;;;
;;;; Every subcommand produces a plist (or a list of them) and hands it here.
;;;; Doing the formatting in one place is what makes --json work everywhere
;;;; rather than in whichever subcommands remembered to implement it.

(in-package #:im.cli)

(defvar *json* nil "True when --json was given.")
(defvar *verbose* nil "True when --verbose was given.")

(defun verbose (control &rest arguments)
  "Write a progress note to stderr, if --verbose is on.

stderr rather than stdout so that `im info --json x.png | jq` keeps working."
  (when *verbose*
    (apply #'format *error-output* control arguments)
    (finish-output *error-output*)))

;;; JSON ----------------------------------------------------------------------

(defun json-key (keyword)
  "IM:DATA-TYPE-BYTE style keywords become lowerCamel-free kebab strings.

Keeping the kebab spelling rather than converting to camelCase: it is what the
Lisp API calls the field, so a script written against one reads against the
other."
  (string-downcase (symbol-name keyword)))

(defun jsonable (value)
  "Convert VALUE into something shasht can serialise.

Keywords become strings -- a colour space is :COLOR-SPACE-RGB in Lisp and
should be \"color-space-rgb\" in JSON, not an object. Plists become hash
tables, since a plist is a list to shasht and would come out as an array."
  (typecase value
    (null :null)
    ((eql t) t)
    (keyword (json-key value))
    ;; STRING must precede VECTOR. A string is a vector of characters, so the
    ;; vector clause matched first and serialised "JPEG" as ["J","P","E","G"].
    (string value)
    (pathname (namestring value))
    (hash-table value)
    (cons
     (if (plist-p value)
         (let ((table (make-hash-table :test #'equal)))
           (loop for (key raw) on value by #'cddr
                 do (setf (gethash (json-key key) table) (jsonable raw)))
           table)
         (mapcar #'jsonable value)))
    (vector (map 'vector #'jsonable value))
    (t value)))

(defun plist-p (list)
  "True for a list that looks like (:KEY value :KEY value ...).

Necessarily a heuristic, and one with a blind spot worth naming: a list whose
elements are ALL keywords is indistinguishable from a plist whose values
happen to be keywords. (:COLOR-MODE-CONFIG-PACKED :COLOR-MODE-CONFIG-TOPDOWN)
is a set of flags, but it reads here as one key and one value.

Rather than guess, callers normalise keyword lists to strings before handing
them over -- see KEYWORD-NAMES. This predicate is only asked about data that
has already been through that."
  (and (listp list)
       (plusp (length list))
       (evenp (length list))
       (keywordp (first list))
       (loop for (key nil) on list by #'cddr always (keywordp key))))

(defun keyword-names (keywords)
  "A list of keywords as a list of strings, so it cannot be read as a plist."
  (mapcar (lambda (k) (if (keywordp k) (string-downcase (symbol-name k)) k))
          keywords))

;;; Human-readable ------------------------------------------------------------

(defun humanize (key)
  (substitute #\Space #\- (string-downcase (symbol-name key))))

(defun print-plist (plist &key (stream *standard-output*) (indent 0))
  "Print a plist as aligned `key: value' lines."
  (let ((width (loop for (key nil) on plist by #'cddr
                     maximize (length (humanize key)))))
    (loop for (key value) on plist by #'cddr
          ;; Explicit spaces rather than ~v,0T. A colinc of 0 divides by zero
          ;; inside the tabulation directive on SBCL, which surfaced as
          ;; "arithmetic error DIVISION-BY-ZERO signalled" from a command that
          ;; had already done all its real work.
          do (write-string (make-string indent :initial-element #\Space) stream)
             (format stream "~vA  " width (humanize key))
             (print-value value stream (+ indent width 2))
             (terpri stream))))

(defun scalar (value)
  (typecase value
    (keyword (string-downcase (symbol-name value)))
    ;; 108.69650268554688d0 is how Lisp writes a double and not how anything
    ;; else reads one. Six significant decimals is plenty for a pixel
    ;; statistic and loses the marker.
    (double-float (format nil "~,6F" value))
    (single-float (format nil "~,6F" value))
    (t value)))

(defun print-value (value stream indent)
  (typecase value
    (null (write-string "-" stream))
    ((eql t) (write-string "yes" stream))
    (keyword (write-string (string-downcase (symbol-name value)) stream))
    (string (write-string value stream))
    (float (write-string (scalar value) stream))
    (pathname (write-string (namestring value) stream))
    (cons
     (cond
       ((plist-p value)
        (terpri stream)
        (print-plist value :stream stream :indent (+ indent 2)))
       ;; A list of plists -- the shape of :frames, :statistics, :regions and
       ;; :libraries -- is a sequence of blocks, not a comma-separated line.
       ;; Formatting it as one produced a paragraph of nested parentheses that
       ;; ran off the right of the terminal.
       ((every #'plist-p value)
        (terpri stream)
        (loop for item in value
              for first = t then nil
              do (unless first (terpri stream))
                 (print-plist item :stream stream :indent (+ indent 2))))
       (t (format stream "~{~A~^, ~}" (mapcar #'scalar value)))))
    (vector (format stream "~{~A~^, ~}" (map 'list #'scalar value)))
    (t (princ value stream))))

(defun emit (data)
  "Render DATA as JSON or as text, according to --json."
  (if *json*
      (progn (shasht:write-json (jsonable data) *standard-output*)
             (terpri *standard-output*))
      (if (and (listp data) (not (plist-p data)))
          (loop for item in data
                for first = t then nil
                do (unless first (terpri))
                   (print-plist item))
          (print-plist data)))
  (finish-output *standard-output*))

(defun emit-table (rows &key headers)
  "Render ROWS -- a list of lists -- as an aligned table, or as JSON."
  (if *json*
      (progn (shasht:write-json (jsonable rows) *standard-output*)
             (terpri *standard-output*))
      (let ((widths (when rows
                      (loop for column below (length (first rows))
                            collect (max (if headers
                                             (length (string (nth column headers)))
                                             0)
                                         (loop for row in rows
                                               maximize (length (princ-to-string
                                                                 (nth column row)))))))))
        (when headers
          (format t "~{~vA~^  ~}~%"
                  (loop for h in headers for w in widths append (list w (string h))))
          (format t "~{~A~^  ~}~%"
                  (mapcar (lambda (w) (make-string w :initial-element #\-)) widths)))
        (dolist (row rows)
          (format t "~{~vA~^  ~}~%"
                  (loop for cell in row for w in widths
                        append (list w (princ-to-string cell)))))))
  (finish-output *standard-output*))
