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
                                     (width height color-mode data-type)
                                   (im:file-read-image-info file index)
                                 `(:width ,width
                                   :height ,height
                                   :color-mode (:space-name ,(im:color-mode-space-name color-mode)
                                                :alpha-p nil
                                                :packed-p nil
                                                :top-down-p nil)
                                   :data-type ,(im:data-type-name data-type))))))))

(image-info #p"/home/mkennedy/Downloads/MultipleFormats.tif")

