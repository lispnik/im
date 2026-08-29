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
          image-attribute-string
          set-attribute-string
          remove-attribute
          with-temporary-attribute
          guess-format))

(defun %namestring (pathname)
  "IM takes a C string; a logical pathname would reach it untranslated."
  (namestring (translate-logical-pathname pathname)))

(defun image-attribute-string (image name)
  "The string value of IMAGE's NAME attribute, or NIL if it has none."
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

(defun %file-attributes (file)
  "Every attribute on FILE as an alist of (NAME VALUE DATA-TYPE COUNT).

Two calls, as IM's API requires: the first with a null array to learn the
count, the second to fill it. The names IM hands back point into its own
attribute table and are copied here immediately -- the header warns they are
invalidated by the next set or remove, and a Lisp string is the only form
worth returning."
  (cffi:with-foreign-object (count :int)
    (im.ffi::%im-file-get-attribute-list file (cffi:null-pointer) count)
    (let ((n (cffi:mem-ref count :int)))
      (when (plusp n)
        (cffi:with-foreign-object (names :pointer n)
          (setf (cffi:mem-ref count :int) n)
          (im.ffi::%im-file-get-attribute-list file names count)
          (loop for i below (cffi:mem-ref count :int)
                for name = (cffi:foreign-string-to-lisp
                            (cffi:mem-aref names :pointer i))
                collect (cons name (%file-attribute file name))))))))

(defun %file-attribute (file name)
  "One attribute's value, decoded by its data type."
  (cffi:with-foreign-objects ((data-type :int) (count :int))
    (let ((pointer (im.ffi::%im-file-get-attribute file name data-type count)))
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
through Babel themselves."
  (flet ((values-of (cffi-type)
           (let ((v (make-array count)))
             (dotimes (i count v)
               (setf (aref v i) (cffi:mem-aref pointer cffi-type i))))))
    (case type
      (:data-type-byte
       (let ((bytes (values-of :unsigned-char)))
         (if (%printable-bytes-p bytes)
             ;; Trailing NUL is a C string terminator, not content.
             (map 'string #'code-char
                  (remove 0 bytes :start (max 0 (1- (length bytes)))))
             bytes)))
      (:data-type-short  (values-of :short))
      (:data-type-ushort (values-of :unsigned-short))
      (:data-type-int    (values-of :int))
      (:data-type-float  (values-of :float))
      (:data-type-double (values-of :double))
      (t (values-of :unsigned-char)))))

(defun %printable-bytes-p (bytes)
  "True when BYTES look like text: printable ASCII, with at most a final NUL."
  (and (plusp (length bytes))
       (every (lambda (b) (or (<= 32 b 126) (member b '(0 9 10 13))))
              bytes)
       ;; A single NUL is a terminator; NULs in the middle mean binary.
       (let ((nul (position 0 bytes)))
         (or (null nul) (= nul (1- (length bytes)))))))

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
  "One named attribute of PATHNAME's first frame, or NIL."
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
