(defpackage #:im-examples.image-copy
  (:use #:common-lisp)
  (:export #:image-copy))

(in-package #:im-examples.image-copy)

;;; image-copy exercises most of the IM representation API

;;; Example usage:

#+nil
(loop for in-file in (directory #p"/usr/share/backgrounds/*.jpg")
      for out-file = (merge-pathnames
                      (make-pathname :name (pathname-name in-file) 
                                     :type "tif")
                      #p"/tmp/")
      do (image-copy in-file out-file "TIFF"))

(defun image-copy
    (input-pathname output-pathname &optional output-format output-compression)
  "Copy and optionally convert one file to another."
  #+windows (im-wmv:format-register-wmv)
  #+windows (im-avi:format-register-avi)
  (im:with-open-file (input-file (im:file-open input-pathname))
      (multiple-value-bind
            (input-format input-compression input-count)
          (im:file-info input-file)
        (im:with-open-file
            (output-file (im:file-new
                          output-pathname
                          (if output-format output-format input-format)))
          (setf (im:file-compression output-file)
                (if output-compression
                    output-compression
                    input-compression))
          (dotimes (i input-count)
            (multiple-value-bind
                  (width height color-mode-config color-space data-type)
                (im:file-read-image-info input-file i)
              (let ((data-size (im:image-data-size
                                width height
                                color-mode-config color-space
                                data-type)))
                (cffi:with-foreign-object
                    (data-ptr :pointer data-size)
                  (im:file-read-image-data input-file data-ptr nil nil)
                  (dolist (attribute (im:file-attributes input-file))
                    (multiple-value-bind
                          (attributes data-type)
                        (im:file-attribute input-file attribute)
                      (setf (im:file-attribute input-file attribute data-type)
                            attributes)))
                  (when (eq color-space :color-space-map)
                    (let ((palette (im:file-palette input-file)))
                      (setf (im:file-palette output-file) palette)))
                  (im:file-write-image-info
                   output-file
                   width height
                   color-mode-config color-space
                   data-type)
                  (im:file-write-image-data output-file data-ptr)))))))))
