(in-package #:im)

(export '(file-open
	  file-open-as
	  file-new
	  file-close
	  call-with-open-file
	  with-open-file
	  file-handle
          file-info
          file-image-info
          file-compression
          file-attributes
          file-palette
          file-read-image-info
          file-write-image-info))

(defun as-filename (pathnane-or-namestring)
  (etypecase pathnane-or-namestring
    (pathname (namestring pathnane-or-namestring))
    (string pathnane-or-namestring)))

(defun file-open (pathname)
  "Opens the file for reading. It must exists. Also reads file
header. It will try to identify the file format."
  (let ((filename (as-filename pathname)))
    (cffi:with-foreign-object
	(error-code-ptr 'im-cffi::error-code)
      (let ((result (im-cffi::%im-file-open filename error-code-ptr)))
	(or (maybe-error (cffi:mem-ref error-code-ptr 'im-cffi::error-code))
	    result)))))

(defun file-open-as (pathname format)
  "Opens the file for reading using a specific format. It must
exists. Also reads file header."
  (let ((filename (as-filename pathname)))
    (cffi:with-foreign-object
	(error-code-ptr 'im-cffi::error-code)
      (let ((result (im-cffi::%im-file-open-as filename format error-code-ptr)))
	(or (maybe-error (cffi:mem-ref error-code-ptr 'im-cffi::error-code))
	    result)))))

(defun file-new (pathname format)
  "Creates a new file for writing using a specific format. If the file
exists will be replaced. It will only initialize the format driver and
create the file, no data is actually written."
  (let ((filename (as-filename pathname)))
    (cffi:with-foreign-object
	(error-code-ptr 'im-cffi::error-code)
      (let ((result (im-cffi::%im-file-new filename format error-code-ptr)))
	(or (maybe-error (cffi:mem-ref error-code-ptr 'im-cffi::error-code))
	    result)))))

(defun file-close (im-file)
  "Closes the file."
  (im-cffi::%im-file-close im-file))

(defun call-with-open-file (func im-file)
  (unwind-protect
       (funcall func im-file)
    (file-close im-file)))

(defmacro with-open-file ((im-file-var im-file) &body body)
  (with-gensyms (im-file1)
    `(let ((,im-file1 ,im-file))
       (call-with-open-file #'(lambda (,im-file-var)
				,@body)
			    ,im-file1))))

(defun file-handle (im-file index)
  "Returns an internal handle. 

INDEX = 0 always returns an IM-BIN-FILE handle, but for some formats
returns NIL because they do not use IM-BIN-FILE (such as AVI and WMV).

INDEX = 1 returns an internal structure used by the format, usually is
a handle to a third party library structure. This is file format
dependent."
  (let ((handle (im-cffi::%im-file-handle im-file index)))
    (when (cffi:null-pointer-p handle)
      handle)))

(defun file-info (im-file)
  "Returns file information as values FORMAT, COMPRESSION and
IMAGE-COUNT. IMAGE-COUNT is the number of images in a stack or the
number of frames in a video/animation or the depth of a volume data.

These values are also available as IM-FILE attributes: \"FileFormat\",
\"FileCompression\" (strings) and \"FileImageCount\" (integer)."
  (cffi:with-foreign-objects
      ((format-ptr :char 10)            ;blind strcpy :-( probably less than 10 though
       (compression-ptr :char 10)       ;from im_file.h
       (image-count-ptr :int))
    (im-cffi::%im-file-get-info im-file format-ptr compression-ptr image-count-ptr)
    (values (cffi:foreign-string-to-lisp format-ptr)
            (cffi:foreign-string-to-lisp compression-ptr)
            (cffi:mem-ref image-count-ptr :int))))

(defun file-compression (im-file)
  "Returns the compression method."
  (nth-value 1 (file-info im-file)))

(defun (setf file-compression) (compression im-file)
  "Changes the write compression method. 

If the compression is not supported, and error will be signaled when
writing.  Use NIL to set the default compression. You can use the
FILE-INFO to retrieve the actual compression but only after
FILE-WRITE-IMAGE-INFO. Only a few formats allow you to change the
compression between frames."
  (if compression
      (cffi:with-foreign-string
          (compression-ptr compression)
        (im-cffi::%im-file-set-info im-file compression-ptr))
      (im-cffi::%im-file-set-info im-file (cffi:null-pointer)))
  compression)

;; (defgeneric (setf file-attribute) (new-value im-file attribute data-type)
;;   (:method ((new-value real) im-file attribute data-type)
;;     (im-cffi::%im-file-set-attribute-real
;;      im-file
;;      attribute
;;      data-type
;;      (coerce new-value 'double-float)))
;;   (:method ((new-value integer) im-file attribute data-type)
;;     (im-cffi::%im-file-set-attribute-integer
;;      im-file
;;      attribute
;;      data-type
;;      (coerce new-value '(signed-byte #.(* 8 (cffi:foreign-type-size :int))))))
;; #+nil  (:method ((new-value string) im-file attribute data-type)
;;     (im-cffi::%im-file-set-attribute-string )))

;; (defun file-attribute (im-file attribute)
;;   (cffi:with-foreign-objects
;;       ((data-type-ptr 'im-cffi::data-type)
;;        (count-ptr :int))
;;     (let* ((value-ptr (im-cffi::%im-file-get-attribute im-file attribute data-type-ptr count-ptr))
;;            (data-type (cffi:mem-ref data-type-ptr 'im-cffi::data-type)))
;;       (values (ecase data-type
;;                 (:data-type-byte (cffi:foreign-string-to-lisp value-ptr))
;;                 (:data-type-short (cffi:mem-ref value-ptr :short))
;;                 (:data-type-ushort (cffi:mem-ref value-ptr :unsigned-short))
;;                 (:data-type-int (cffi:mem-ref value-ptr :int))
;;                 (:data-type-float (cffi:mem-ref value-ptr :float))
;;                 (:data-type-double (cffi:mem-ref value-ptr :double))
;;                 (:data-type-cfloat
;;                  (complex (cffi:mem-aref value-ptr :float 0)
;;                           (cffi:mem-aref value-ptr :float 1)))
;;                 (:data-type-cdouble
;;                  (complex (cffi:mem-aref value-ptr :double 0)
;;                           (cffi:mem-aref value-ptr :double 1))))
;;               (cffi:mem-ref count-ptr :int)))))


;; (defun (setf attribute-string) (new-value im-image attribute)
;;   (im-cffi::%im-image-set-attrib-string im-image attribute new-value)
;;   new-value)
;; (defun attribute-string (im-image attribute)
;;   (im-cffi::%im-image-get-attrib-string im-image attribute))

(defun file-attributes (im-file)
  "Return file attribute names as a list of strings."
  (cffi:with-foreign-object
      (attrib-count-ptr :int)
    (im-cffi::%im-file-get-attribute-list im-file (cffi:null-pointer) attrib-count-ptr)
    (let ((attrib-count (cffi:mem-aref attrib-count-ptr :int)))
      (cffi:with-foreign-object
          (attrib-ptr :pointer attrib-count)
        (im-cffi::%im-file-get-attribute-list im-file attrib-ptr attrib-count-ptr)
        (loop for i below attrib-count
              collect (cffi:foreign-string-to-lisp
                       (cffi:mem-aref attrib-ptr :pointer i)))))))

(defun file-palette (im-file)
  "Returns the palette as a non-empty SEQUENCE of up to 256 colors or
NIL if there is no palette."
  (cffi:with-foreign-objects
      ((palette-ptr :long 256)
       (count-ptr :int))
    (im-cffi::%im-file-get-palette im-file palette-ptr count-ptr)
    (let ((count (cffi:mem-ref count-ptr :int)))
      (unless (zerop count)
        (loop with palette = (make-array count :element-type '(signed-byte #.(* 8 (cffi:foreign-type-size :int))))
              for i below count
              do (setf (aref palette i)
                       (cffi:mem-aref palette-ptr :int i))
              finally (return palette))))))

(defun (setf file-palette) (new-palette im-file)
  "Changes the palette to to NEW-PALETTE. NEW-PALETTE must be
non-empty SEQUENCE of up to 256 colors."
  (let ((count (length new-palette)))
    (assert (<= 1 count 256))
    (cffi:with-foreign-objects
        ((palette-ptr :int (length new-palette))
         (count-ptr :int))
      (setf (cffi:mem-ref count-ptr :int) count)
      (loop for i below count
            do (setf (cffi:mem-aref palette-ptr :int i)
                     (elt new-palette i)))
      (im-cffi::%im-file-set-palette im-file palette-ptr count-ptr)
      new-palette)))

(defun file-read-image-info (im-file &optional (index 0))
  "Reads the image header if any and returns image information.
Returns the values WIDTH, HEIGHT, COLOR-SPACE, COLOR-MODE-CONFIG and
DATA-TYPE.

Reads also the extended image attributes, so other image attributes
will be available only after calling this function. An condition of
FILE-ACCESS-ERROR is signalled upon error. INDEX specifies the image
number between 0 and IMAGE-COUNT - 1.

Some drivers read only in sequence, so INDEX can be ignored by the
format driver. This function must be called at least once, check each
format documentation."
  (cffi:with-foreign-objects
      ((width-ptr :int)
       (height-ptr :int)
       (color-mode-ptr :int)
       (data-type-ptr 'im-cffi::data-type))
    (maybe-error
     (im-cffi::%im-file-read-image-info im-file index width-ptr height-ptr color-mode-ptr data-type-ptr))
    (let ((color-mode (cffi:mem-ref color-mode-ptr :int)))
      (values (cffi:mem-ref width-ptr :int)
              (cffi:mem-ref height-ptr :int)
              (color-mode-space color-mode)
              (color-mode-config color-mode)
              (cffi:mem-ref data-type-ptr 'im-cffi::data-type)))))

(defun file-write-image-info (im-file)
  ;; FIXME
  )

(defun file-read-image-data (im-file)
  ;; FIXME
  )

(defun file-write-image-data (im-file)
  ;; FIXME
  )

#+nil
(let ((im-file (file-open "exif-samples/tiff/Jobagent.tiff")))
  (unwind-protect
       (progn
         (file-palette im-file))
    (file-close im-file)))

