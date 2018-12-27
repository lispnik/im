(defpackage #:im-examples.image-info
  (:use #:common-lisp)
  (:export #:image-info))

(in-package #:im-examples.image-info)

(defun image-metadata (file index)
  (multiple-value-bind 
        (width height
         color-mode-config color-space
         data-type)
      (im:file-read-image-info file index)
    `(:width ,width
      :height ,height
      :color-mode (:color-space ,color-space
                   :color-mode-config ,color-mode-config)
      :data-type ,data-type
      :data-size ,(im:image-data-size
                   width height
                   color-mode-config color-space
                   data-type)
      :attributes ,(loop with attributes = (im:file-attributes file)
                         for attribute in attributes
                         collect (cons attribute
                                       (multiple-value-list
                                        (im:file-attribute file attribute)))))))

(defun image-info (pathname)
  (im:with-open-file (file (im:file-open (if (pathnamep pathname) (namestring pathname) pathname)))
    (multiple-value-bind
          (format compression count)
        (im:file-info file)
      `(:pathname ,pathname
        :format ,format
        :compression ,compression
        :count ,count
        :images
        ,(loop for index below count
               collect (image-metadata file index))))))

#+nil
(loop for file in (directory #p"/usr/share/backgrounds/*.jpg")
      collect (image-info file))



