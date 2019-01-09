(defpackage #:im-capture
  (:use #:common-lisp
	#:cffi
	#:serapeum)
  (:export #:invalid-device-error
	   #:no-device-error
	   #:connection-error
	   #:device-configuration-error
	   #:capture-error
	   #:device-count
	   #:device-description
	   #:device-extended-description
	   #:device-path
	   #:device-vendor-info
	   #:reload-devices
	   #:release-devices
	   #:create
	   #:destroy
	   #:connect
	   #:connected-device
	   #:disconnect
	   #:dialog-count
	   #:show-dialog
	   #:dialog-description
	   #:set-in-out
	   #:format-count
	   #:format
	   #:current-format
	   #:image-size
	   #:capture-frame
	   #:capture-one-frame
	   #:live
	   #:live-p)
  (:shadow #:format))

(in-package #:im-capture)

(define-condition invalid-device-error () ())
(define-condition no-device-error () ())
(define-condition connection-error () ())
(define-condition device-configuration-error () ())
(define-condition capture-error () ())

(defalias device-count #'im-capture-cffi::%im-video-capture-device-count
  "Returns the number of available devices.")

(defun device-description (device)
  "Returns the device description. Signals INVALID-DEVICE-ERROR if the
device is invalid."
  (or (im-capture-cffi::%im-video-capture-device-desc device)
      (error 'invalid-device-error)))

(defun device-extended-description (device)
  "returns the extended device description. signals
invalid-device-error if the device is invalid."
  (or (im-capture-cffi::%im-video-capture-device-ex-desc device)
      (error 'invalid-device-error)))

(defun device-path (device)
  "returns the device path configuration. this is a unique
string. signals invalid-device-error if the device is invalid."
  (or (im-capture-cffi::%im-video-capture-device-path device)
      (error 'invalid-device-error)))

(defun device-vendor-info (device)
  "returns the vendor information. signals invalid-device-error if the
device is invalid."
  (or (im-capture-cffi::%im-video-capture-device-vendor-info device)
      (error 'invalid-device-error)))

(defalias reload-devices #'im-capture-cffi::%im-video-capture-reload-devices
  "reload the device list. the devices can be dynamically removed or
added to the system. returns the number of available devices.")

(defalias release-devices #'im-capture-cffi::%im-video-capture-release-devices
  "release the device list. useful is you need to track leak errors in
your application.")

(defun create ()
  "creates a new video capture context. 

signals no-device-error if no capture device is available or, if on
windows the directx version is older than 8."
  (let ((context (im-capture-cffi::%im-video-capture-create)))
    (if (cffi:null-pointer-p context)
	(error 'no-device-error)
	context)))

(defun destroy (context)
  "destroys a video capture context returned by create."
  (im-capture-cffi::%im-video-capture-destroy context))

(defun connect (context device)
  "connects to a capture device. 

more than one video capture context can be created but they must be
connected to different devices. if the context is connected it will
disconnect first."
  (check-type device (integer 0 *))
  (let ((result (im-capture-cffi::%im-video-capture-connect context device)))
    (when (zerop result)
	(error 'connection-error))))

(defun connected-device (context)
  "returns the device number connected to an video capture context or
nil if it is not connected."
  (let ((connected-device (im-capture-cffi::%im-video-capture-connect context -1)))
    (if (= connected-device -1)
	nil
	connected-device)))

(defalias disconnect #'im-capture-cffi::%im-video-capture-disconnect
  "disconnect from a capture device context.")

(defalias dialog-count #'im-capture-cffi::%im-video-capture-dialog-count
  "returns the number of available configuration dialogs.")

(defun show-dialog (context dialog &optional (parent (cffi:null-pointer)))
  "displays a configuration modal dialog of the connected device

in windows, the capturing will be stopped in some cases. in windows,
parent is a hwnd of a parent window. dialog can be from 0 to
dialog-count.

signals device-configuration-error on error."
  (im-capture-cffi::%im-video-capture-show-dialog context dialog parent))

(defalias dialog-description #'im-capture-cffi::%im-video-capture-dialog-desc
  "returns the description of a configuration dialog. dialog can be
from 0 to dialog-count.")

(defun set-in-out (context input output &optional (cross-index 1))
  "allows to control the input and output of devices that have
multiple input and outputs.
 
the cross index controls in which stage the input/output will be
set. usually use 1, but some capture boards has a second stage. in
direct x it controls the cross-bars. 

signals device-configuration-error on error."
  (when (im-capture-cffi::%im-video-capture-set-in-out
	 context
	 input
	 output
	 cross-index)
    (error 'device-configuration-error)))

(defun format-count (context)
  "returns the number of available video formats. signals
device-configuration-error on error."
  (let ((count (im-capture-cffi::%im-video-capture-format-count context)))
    (if (zerop count)
	(error 'device-configuration-error)
	count)))

(defun format (context format)
  "returns information about the video format as values width height
and description. signals device-configuration-error on error."
  (cffi:with-foreign-objects
      ((width-ptr :int)
       (height-ptr :int)
       (description-ptr :unsigned-char 20))
    (loop for i below 20
	  do (setf (cffi:mem-aref description-ptr :unsigned-char i) 0))
    (let ((result (im-capture-cffi::%im-video-capture-get-format
		   context
		   format
		   width-ptr
		   height-ptr
		   description-ptr)))
      (if result
	  (values (cffi:mem-ref width-ptr :int)
		  (cffi:mem-ref height-ptr :int)
		  (cffi:foreign-string-to-lisp description-ptr))
	  (error 'device-configuration-error)))))

(defun (setf format) (format context)
  "changes the video format of the connected device. 

should not work for dv devices. use (setf image-size) only. when the
format is changed in the dialog, for some formats the returned format
is the preferred format, not the current format. this will not affect
the color-mode-configuration or color-space of the captured image.

signals device-configuration-error on error."
  (check-type format (integer 0 *))
  (if (zerop (im-capture-cffi::%im-video-capture-set-format context format))
      (error 'device-configuration-error)
      format))

(defun current-format (context)
  "returns the current format index. signals
device-configuration-error on error."
  (let ((result (im-capture-cffi::%im-video-capture-set-format context -1)))
    (if (= result -1)
	(error 'device-configuration-error)
	result)))

(defun image-size (context)
  "returns the current image size of the connected device as width and
height values. signals device-configuration-error on error."
  (cffi:with-foreign-objects
      ((width-ptr :int)
       (height-ptr :int))
    (im-capture-cffi::%im-video-capture-get-image-size context width-ptr height-ptr)
    (let ((result (cffi:mem-ref width-ptr :int)))
      (if (zerop result)
	  (error 'device-configuration-error)
	  (values (cffi:mem-ref width-ptr :int)
		  (cffi:mem-ref height-ptr :int))))))

;; todo int im_decl imvideocapturesetimagesize(imvideocapture* vc, int width, int height);

(defun %verify-capture-frame-data-vector (context data color-mode-config color-space)
  (assert (member color-space '(:color-space-gray :color-space-rgb)))
  (assert (equal (array-element-type data) '(unsigned-byte 8)))
  (multiple-value-bind (width height)
      (image-size context)
    (let ((expected-data-size
	    (im:image-data-size width height
				color-mode-config color-space
				:data-type-byte)))
      (assert (>= (length data) expected-data-size)))))

(defun capture-frame (context data color-mode-config color-space &optional timeout)
  (%verify-capture-frame-data-vector context data color-mode-config color-space)
  (im-capture-cffi::%im-video-capture-frame
   context
   (static-vectors:static-vector-pointer data)
   (im::%encode-color-mode color-mode-config color-space)
   (if timeout timeout -1)))

(defun capture-one-frame (context data color-mode-config color-space)
  (%verify-capture-frame-data-vector context data color-mode-config color-space)
  (im-capture-cffi::%im-video-capture-one-frame
   context
   (static-vectors:static-vector-pointer data)
   (im::%encode-color-mode color-mode-config color-space)))

(defun live (context live-p)
  (let ((result (im-capture-cffi::%im-video-capture-live context (if live-p 1 0))))
    (if (zerop result)
	(error 'capture-error))))

(defun live-p (context)
  "Returns T if the context is live (capturing), NIL if not live."
  (not (zerop (im-capture-cffi::%im-video-capture-live context -1))))

#+nil
(let ((context (create)))
  (flet ((checksum (data)
	   (loop with count = (length data)
		 for i below count
		 sum (aref data i))))
    (unwind-protect
	 (unwind-protect
	      (progn
		(connect context 0)
		(live context t)
		(sleep 4)
		(multiple-value-bind
		      (width height)
		    (image-size context)
		  (let* ((color-mode-config '(:color-mode-config-packed))
			 (color-space :color-space-rgb)
			 (data-type :data-type-byte)
			 (data-size (im:image-data-size width height color-mode-config color-space data-type))
			 (data (static-vectors:make-static-vector data-size :element-type '(unsigned-byte 8) :initial-element 0)))
		    (unwind-protect
			 (progn
			   (cl:format t "~&before ~A" (checksum data))
			   (capture-frame context data color-mode-config color-space)
			   (cl:format t "~&after ~A" (checksum data))
			   (finish-output))
		      (static-vectors:free-static-vector data))))
		(live context nil))
	   (disconnect context))
      (destroy context))))
