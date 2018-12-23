(in-package #:im)

(export '(file-open
	  file-open-as
	  file-new
	  file-close
	  call-with-open-file
	  with-open-file
	  file-handle
          file-info
          file-read-image-info))

(defun as-filename (pathnane-or-namestring)
  (etypecase pathnane-or-namestring
    (pathname (namestring pathnane-or-namestring))
    (string pathnane-or-namestring)))

(defun file-open (pathname)
  (let ((filename (as-filename pathname)))
    (cffi:with-foreign-object
	(error-code-ptr :int)
      (let ((result (im-cffi::%im-file-open filename error-code-ptr)))
	(or (maybe-error (cffi:mem-ref error-code-ptr :int))
	    result)))))

(defun file-open-as (pathname format)
  (let ((filename (as-filename pathname)))
    (cffi:with-foreign-object
	(error-code-ptr :int)
      (let ((result (im-cffi::%im-file-open-as filename format error-code-ptr)))
	(or (maybe-error (cffi:mem-ref error-code-ptr :int))
	    result)))))

(defun file-new (pathname format)
  (let ((filename (as-filename pathname)))
    (cffi:with-foreign-object
	(error-code-ptr :int)
      (let ((result (im-cffi::%im-file-new filename format error-code-ptr)))
	(or (maybe-error (cffi:mem-ref error-code-ptr :int))
	    result)))))

(defun file-close (im-file)
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
  (im-cffi::%im-file-handle im-file index))

(defun file-info (im-file)
  (cffi:with-foreign-objects
      ((format-ptr :char 10)            ;FIXME magic numbers here
       (compression-ptr :char 20)
       (image-count-ptr :int))
    (im-cffi::%im-file-get-info im-file format-ptr compression-ptr image-count-ptr)
    (values (cffi:foreign-string-to-lisp format-ptr)
            (cffi:foreign-string-to-lisp compression-ptr)
            (cffi:mem-ref image-count-ptr :int))))

;;; TODO aka setinfo 
;; (defun (setf file-compression) (compression im-file))
;;; TODO
;; (defun attribute (im-file name data-type count))

;; (deftype attribute-type ()
;;   `(member (unsigned-byte 8)
;;            (signed-byte 16)
;;            (unsigned-byte 16)
;;            (signed-byte 32)
;;            single-float
;;            double-float
;;            complex
;;            string))

;; (defun (setf attribute) (value im-file name type)
;;   (check-type name string)
;;   (check-type type attribute-type)
;;   (etypecase type
;;     ((unsigned-byte 8)
;;      (cffi:with-foreign-object
;;          (data-ptr :uint8)
;;        (setf (cffi:mem-ref data-ptr :uint8) (coerce value type))
;;        (im-cffi::%im-file-set-attribute im-file name im-cffi::data-type-byte 1 data-ptr))))
;;   value)

(defun attribute-list (im-file)
  (let ((count
          (cffi:with-foreign-object
              (count-ptr :int)
            (im-cffi::%im-file-get-attribute-list
             im-file
             (cffi:null-pointer)
             count-ptr)
            (cffi:mem-ref count-ptr :int))))
    (cffi:with-foreign-objects
        ((attributes-ptr :pointer count)
         (count-ptr :int))
      (im-cffi::%im-file-get-attribute-list im-file attributes-ptr count-ptr)
      (loop for i below count
            collect (cffi:foreign-string-to-lisp
                     (cffi:mem-aref attributes-ptr :pointer i))))))

(defun palette (im-file)
  (cffi:with-foreign-objects
      ((palette-ptr :long 256)
       (count-ptr :int))
    (im-cffi::%im-file-get-palette im-file palette-ptr count-ptr)
    (let ((count (cffi:mem-ref count-ptr :int)))
      (unless (zerop count)
        (loop with palette = (make-array count :element-type 'integer)
              for i below count
              do (setf (aref palette i) (cffi:mem-aref palette-ptr :long i))
              finally (return palette))))))

(defun (setf palette) (palette im-file)
  (let ((length (length palette)))
    (assert (<= 1 length 256))
    (cffi:with-foreign-object
        (palette-ptr :long length)
      (loop for i below (length palette)
            do (setf (cffi:mem-aref palette-ptr :long i) (elt palette i))
            finally (im-cffi::%im-file-set-palette im-file palette-ptr length))
      palette)))


;; int imFileReadImageInfo(imFile* ifile, int index, int *width, int *height, int *file_color_mode, int *file_data_type);

(defun file-read-image-info (im-file index)
  (cffi:with-foreign-objects
      ((width-ptr :int)
       (height-ptr :int)
       (color-mode-ptr :int)
       (file-data-type-ptr :int))
    (im-cffi::%im-file-read-image-info im-file index width-ptr height-ptr color-mode-ptr file-data-type-ptr)
    (values (cffi:mem-ref width-ptr :int)
            (cffi:mem-ref height-ptr :int)
            (cffi:mem-ref color-mode-ptr :int)
            (cffi:mem-ref file-data-type-ptr :int))))

;;; image

(defun file-image-save (filename format im-image)
  (maybe-error (im-cffi::%im-file-image-save filename format im-image)))


#+nil
(with-open-file (file (file-open (namestring (asdf:system-relative-pathname :im "examples/barbara.bmp"))))
  (let* ((palette (palette file))
         (new-palette 
           (map 'simple-array
                #'(lambda (color)
                    (let* ((r (ldb (byte 8 0) color))
                           (g (ldb (byte 8 8) color))
                           (b (ldb (byte 8 16) color)))
                      (+ (floor r 4)
                         (ash (floor g 4) 8)
                         (ash (floor b 4) 16))))
                palette)))
    (setf (palette file) new-palette)
    (file-image-save "/tmp/foo.gif" "GIF" file)))

(define-constant
    +data-type+
    '((0 . (unsigned-byte 8))
      (1 . (signed-byte 16))
      (2 . (unsigned-byte 16))
      (3 . (signed-byte 32))
      (4 . single-float)
      (5 . double-float)
      (6 . (complex single-float))
      (7 . (complex double-float)))
  :test #'equalp)

(define-constant
    +color-space+
    #(:color-space-rgb
      :color-space-map
      :color-space-gray
      :color-space-binary
      :color-space-cmyk
      :color-space-ycbcr
      :color-space-lab
      :color-space-luv
      :color-space-xyz)
  :test #'equalp)

(define-constant +color-mode-config+
    '((#x100 . :color-mode-alpha)
      (#x200 . :color-mode-packed)
      (#x400 . :color-mode-top-down))
  :test #'equalp)



