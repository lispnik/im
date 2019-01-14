(defpackage #:im-examples.cheese
  (:use #:common-lisp)
  (:export #:cheese))

(in-package #:im-examples.cheese)

(defun cheese ()
  (let ((context (im-capture:create)))
    (unwind-protect
	 (unwind-protect
	      (progn
	        ;; Use first device (probably webcam)
	        (im-capture:connect context 0)
	        (im-capture:live context t)
	        (cl:format t "Say CHEESE! (and press enter)~%")
	        (finish-output)
	        (read-line)
	        (multiple-value-bind
		      (width height)
		    (im-capture:image-size context)
		  (let* ((color-mode-config '(:color-mode-config-packed))
			 (color-space :color-space-gray)
			 (data-type :data-type-byte)
			 (data-size (im:image-data-size width height color-mode-config color-space data-type))
			 (data (static-vectors:make-static-vector data-size :element-type '(unsigned-byte 8) :initial-element 0)))
		    (unwind-protect
			 (uiop:with-temporary-file
			     (:stream stream
			      :pathname temp-pathname
                              :keep t
                              :direction :output
                              :element-type '(unsigned-byte 8) :type "dat")
			   (im-capture:capture-frame context data color-mode-config color-space)
			   (write-sequence data stream)
			   (format t "~A~%~A~%~A~%~A~%~Ax~A~%"
				   temp-pathname
				   color-mode-config color-space
				   data-type
				   width height)
)
		      (static-vectors:free-static-vector data))))
	        (im-capture:live context nil))
	   (im-capture:disconnect context))
      (im-capture:destroy context))))
