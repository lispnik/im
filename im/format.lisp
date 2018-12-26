(in-package #:im)

(export '(format-register-internal
          format-remove-all
          format-list
          format-info
          format-info-extra
          format-compressions
          format-can-write-image-p))

(defalias format-register-internal #'im-cffi::%im-format-register-internal
  "Registers all the internal formats. 

It is automatically called internally when a format is accessed, but
can be called to force the internal formats to be registered before
other formats. Notice that additional formats when registered will be
registered before the internal formats if FORMAT-REGISTER-INTERNAL is
not called yet.  To control the register order is useful when two
format drivers handle the same format. The first registered format
will always be used first.")

(defalias format-remove-all #'im-cffi::%im-format-remove-all
  "Remove all registered formats. Call this if you are checking memory
leaks.")

(defun format-list ()
  "Returns a list of the registered formats."
  (cffi:with-foreign-objects
      ((format-list-ptr :pointer 50)
       (format-count-ptr :int))
    (im-cffi::%im-format-list format-list-ptr format-count-ptr)
    (loop with count = (cffi:mem-ref format-count-ptr :int)
          for i below count
          collect (cffi:foreign-string-to-lisp
                   (cffi:mem-aref format-list-ptr :pointer i)))))

(defun format-info (format)
  "Retuns the format description as values DESCRIPTION, EXT-LIST,
CAN-SEQUENCE-P. e.g.

\"Tagged Image File Format\", (\"*.tif\" \"*.tiff\"), T

A FILE-ACCESS-ERROR is signaled on error."
  (cffi:with-foreign-objects
      ((desc-ptr :char 50)
       (ext-ptr :char 50)
       (can-sequence-ptr :boolean))
    (maybe-error
     (im-cffi::%im-format-info format desc-ptr ext-ptr can-sequence-ptr))
    (flet ((split-exts (exts)
             (split-sequence:split-sequence #\; exts :remove-empty-subseqs t)))
      (values (cffi:foreign-string-to-lisp desc-ptr)
              (split-exts (cffi:foreign-string-to-lisp ext-ptr))
              (cffi:mem-aref can-sequence-ptr :boolean)))))

(defun format-info-extra (format)
  "Returns the format information of the third party library used to
support the format. Typically library version information."
  (cffi:with-foreign-object
      (extra-ptr :char 50)
    (maybe-error (im-cffi::%im-format-info-extra format extra-ptr))
    (cffi:foreign-string-to-lisp extra-ptr)))

(mapcar #'(lambda (format)
            (cons (multiple-value-list (format-info format))
                  (format-info-extra format)))
        (format-list))

(defun format-compressions
    (format &optional color-mode-config-list color-space data-type)
  "Returns a list of format compressions for an (optionaly provided)
COLOR-MODE-CONFIG-LIST, COLOR-SPACE and DATA-TYPE. e.g.

(format-compressions
 \"TIFF\"
 '(:color-mode-config-alpha)
 :color-space-rgb
 :data-type-short)

=> (\"NONE\" \"LZW\" \"ADOBEDEFLATE\" \"RLE\" \"DEFLATE\")

(format-compressions \"TIFF\")

=> (\"NONE\" \"CCITTRLE\" \"CCITTFAX3\" \"CCITTFAX4\" \"LZW\" 
    \"JPEG\" \"ADOBEDEFLATE\" \"NEXT\" \"CCITTRLEW\" \"RLE\" 
    \"THUNDERSCAN\" \"PIXARLOG\" \"DEFLATE\" \"SGILOG\" 
    \"SGILOG24\")"
  (let* ((ignore-color-mode (or (null color-mode-config-list)
                                (null color-space)))
         (color-mode (if ignore-color-mode
                         -1
                         (%encode-color-mode
                          color-mode-config-list color-space)))
         (data-type (if data-type
                        (cffi:foreign-enum-value 'im-cffi::data-type data-type)
                        -1)))
    (cffi:with-foreign-objects
        ((compressions-ptr :pointer 50)
         (compressions-count-ptr :int))
      (im-cffi::%im-format-compressions
       format
       compressions-ptr
       compressions-count-ptr
       color-mode
       data-type)
      (loop with count = (cffi:mem-ref compressions-count-ptr :int)
            for i below count
            collect (cffi:foreign-string-to-lisp
                     (cffi:mem-aref compressions-ptr :pointer i))))))

(defun format-can-write-image-p
    (format compression color-mode-config-list color-space data-type)
  "Checks if the format support the given image class at the given
compression. Signals a FILE-ACCESS-ERROR condition on an error."
  (let ((color-mode (%encode-color-mode color-mode-config-list color-space)))
    (im-cffi::%im-format-can-write-image
     format
     compression
     color-mode
     data-type)))
