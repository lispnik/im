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
          file-attribute
          file-attribute-string
          file-attributes
          file-palette
          file-read-image-info
          file-write-image-info))

(defun %as-filename (pathnane-or-namestring)
  (etypecase pathnane-or-namestring
    (pathname (namestring pathnane-or-namestring))
    (string pathnane-or-namestring)))

(defun file-open (pathname)
  "Opens the file for reading. It must exists. Also reads file
header. It will try to identify the file format."
  (let ((filename (%as-filename pathname)))
    (cffi:with-foreign-object
	(error-code-ptr 'im-cffi::error-code)
      (let ((result (im-cffi::%im-file-open filename error-code-ptr)))
	(or (maybe-error (cffi:mem-ref error-code-ptr 'im-cffi::error-code))
	    result)))))

(defun file-open-as (pathname format)
  "Opens the file for reading using a specific format. It must
exists. Also reads file header."
  (let ((filename (%as-filename pathname)))
    (cffi:with-foreign-object
	(error-code-ptr 'im-cffi::error-code)
      (let ((result (im-cffi::%im-file-open-as filename format error-code-ptr)))
	(or (maybe-error (cffi:mem-ref error-code-ptr 'im-cffi::error-code))
	    result)))))

(defun file-new (pathname format)
  "Creates a new file for writing using a specific format. If the file
exists will be replaced. It will only initialize the format driver and
create the file, no data is actually written."
  (let ((filename (%as-filename pathname)))
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

(defun (setf file-attribute) (value-or-values im-file attribute data-type)
  ;; TODO
  )

(defun %complex-attributes (attribute-ptr count data-type)
  ;; complex attributes are pairs of floats or doubles of real and
  ;; imaginary parts
  (let ((cffi-type (ecase data-type
                     (:data-type-cfloat :float)
                     (:data-type-cdouble :double))))
    (loop for i by 2
          repeat count
          collect (complex (cffi:mem-aref attribute-ptr cffi-type i)
                           (cffi:mem-aref attribute-ptr cffi-type (1+ i))))))

(defun %attributes (attribute-ptr count data-type)
  (if (member data-type '(:data-type-cfloat :data-type-cdouble))
      (%complex-attributes attribute-ptr count data-type)
      (let ((cffi-type (ecase data-type
                         (:data-type-byte :uint8)
                         (:data-type-short :int16)
                         (:data-type-ushort :uint16)
                         (:data-type-int :int32)
                         (:data-type-float :float)
                         (:data-type-double :double))))
        (loop with attributes = (make-array count)
              for i below count
              do (setf (aref attributes i)
                       (cffi:mem-aref attribute-ptr cffi-type i))
              finally (return attributes)))))

(defun file-attribute (im-file attribute)
  "Return the value, or values of ATTRIBUTE within IM-FILE. If there
are more than one value for an attribute, then they are returned as a
SEQUENCE of values."
  (cffi:with-foreign-objects
      ((data-type-ptr 'im-cffi::data-type)
       (count-ptr :int))
    (let* ((value-ptr (im-cffi::%im-file-get-attribute im-file attribute data-type-ptr count-ptr))
           (data-type (cffi:mem-ref data-type-ptr 'im-cffi::data-type))
           (count (cffi:mem-ref count-ptr :int)))
          (let ((attributes (%attributes value-ptr count data-type)))
            (values
             (if (= count 1) (aref attributes 0) attributes)
             data-type
             count)))))

(defun (setf file-attribute-string) (new-value im-file attribute)
  "Set the string value of ATTRIBUTE."
  (im-cffi::%im-file-set-attribute-string im-file attribute new-value)
  new-value)

(defun file-attribute-string (im-image attribute)
  "Get the string value of ATTRIBUTE."
  (im-cffi::%im-image-get-attrib-string im-image attribute))

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
Returns the values WIDTH, HEIGHT, COLOR-MODE-CONFIG-LIST, COLOR-SPACE
and DATA-TYPE.

Reads also the extended image attributes, so other image attributes
will be available only after calling this function. An condition of
IM-ERROR is signalled upon error. INDEX specifies the image number
between 0 and IMAGE-COUNT - 1.

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
              (color-mode-config color-mode)
              (color-mode-space color-mode)
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

