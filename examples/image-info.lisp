(defpackage #:im-examples.image-info
  (:use #:common-lisp)
  (:export #:image-info))

(in-package #:im-examples.image-info)

(defun image-info (pathname)
  (im:with-open-file (file (im:file-open (if (pathnamep pathname) (namestring pathname) pathname)))
    (multiple-value-bind
          (format compression count)
        (im:file-info file)
      `(:pathname ,pathname
        :format ,format
        :compression ,compression
        :count ,count
        :images ,(loop for index below count
                       collect (multiple-value-bind 
                                     (width height color-mode datatype)
                                   (im:file-read-image-info file index)
                                 `(:width ,width
                                   :height ,height
                                   :color-mode ,color-mode
                                   :data-type ,datatype)))))))

(image-info #p"/home/mkennedy/Downloads/multipage_tif_example.tif")

