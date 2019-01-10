(defpackage #:im-examples.glut-capture
  (:use #:common-lisp)
  (:export #:glut-capture))

(in-package #:im-examples.glut-capture)

;;; translation of http://webserver2.tecgraf.puc-rio.br/im/examples/glut_capture.c

(defvar my-video-cap nil)
(defvar image nil)
(defvar gl-data nil)

(cffi:defcallback display :void ()
  (glut:swap-buffers))

(cffi:defcallback reshape :void ((w :int) (h :int))
  (gl:viewport 0 0 w h))

(cffi:defcallback idle :void ()
  (when (im-capture:live-p my-video-cap)
    (im-capture:capture-frame
     my-video-cap
     (cffi:mem-ref (im-image:data image) :pointer)
     '() :color-space-rgb 1000)
    ;; TODO
    ))

(defun update-buffer ()
  (multiple-value-bind
	(width height)
      (im-capture:image-size my-video-cap)
    (unless (and (= width (im-image:width image))
		 (= height (im-image:height image)))
      (im-image:reshape image width height)
      (cffi:foreign-free gl-data)
      (setf gl-data (cffi:foreign-alloc :unsigned-char :count (im-image:size image)))
      (glut:reshape-window (im-image:width image) (im-image:height image))
      ;; (gl:init)???
      )))

(cffi:defcallback parsefunckey :void ((key :int) (x :int) (y :int))
  (declare (ignore x y))
  (im-capture:live my-video-cap nil)
  (ignore-errors (im-capture:show-dialog my-video-cap (1- key)))
  (update-buffer)
  (im-capture:live my-video-cap t)  )

(defun get-capture ()
  (let ((count (im-capture:device-count)))
    (loop for i below count
	  do (format t "  ~A" (im-capture:device-description i)))
    (format t "> ~%")
    (let ((selection (read)))
      (assert (<= 0 selection (1- count)))
      selection)))

(defun init-capture ()
  (setf my-video-cap (im-capture:create))
  (handler-case
      (im-capture:connect my-video-cap (get-capture))
    (error (c)
      (declare (ignore c))
      (im-capture:destroy my-video-cap)))
  (handler-case
      (im-capture:live my-video-cap t)
    (error (c)
      (declare (ignore c))
      (im-capture:disconnect my-video-cap)
      (im-capture:destroy my-video-cap)))
  (multiple-value-bind
	(width height)
      (im-capture:image-size my-video-cap)
    (setf image (im-image:create width height :color-space-rgb :data-type-byte))
    (setf gl-data (cffi:foreign-alloc :unsigned-char :count (im-image:size image)))))

(defun glut-capture ()
  (handler-case
      (init-capture)
    (error (c)
      (declare (ignore c))
      (return-from glut-capture)))
  (glut:init "glue-capture")
  (glut:init-display-mode :rgb :double)
  (glut:init-window-position 100 100)
  (glut:init-window-size (im-image:width image) (im-image:height image))
  (glut:create-window "glut-capture")
  (gl:clear-color 0 0 0 1)
  (%gl:pixel-store-i :unpack-alignment 1)
  (glut:display-func (cffi:callback display))
  (glut:reshape-func (cffi:callback reshape))
  (glut:special-func (cffi:callback parsefunckey))
  ;; glInit??
  (glut:main-loop))
