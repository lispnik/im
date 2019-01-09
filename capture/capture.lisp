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
  "Returns the extended device description. Signals
INVALID-DEVICE-ERROR if the device is invalid."
  (or (im-capture-cffi::%im-video-capture-device-ex-desc device)
      (error 'invalid-device-error)))

(defun device-path (device)
  "Returns the device path configuration. this is a unique
string. Signals INVALID-DEVICE-ERROR if the device is invalid."
  (or (im-capture-cffi::%im-video-capture-device-path device)
      (error 'invalid-device-error)))

(defun device-vendor-info (device)
  "Returns the vendor information. Signals INVALID-DEVICE-ERROR if the
device is invalid."
  (or (im-capture-cffi::%im-video-capture-device-vendor-info device)
      (error 'invalid-device-error)))

(defalias reload-devices #'im-capture-cffi::%im-video-capture-reload-devices
  "Reload the device list. The devices can be dynamically removed or
added to the system. Returns the number of available devices.")

(defalias release-devices #'im-capture-cffi::%im-video-capture-release-devices
  "Release the device list. Useful is you need to track leak errors in
your application.")

(defun create ()
  "Creates a new video capture context. 

Signals NO-DEVICE-ERROR if no capture device is available or, if on
windows the directx version is older than 8."
  (let ((context (im-capture-cffi::%im-video-capture-create)))
    (if (cffi:null-pointer-p context)
	(error 'no-device-error)
	context)))

(defun destroy (context)
  "Destroys a video capture context returned by create."
  (im-capture-cffi::%im-video-capture-destroy context))

(defun connect (context device)
  "Connects to a capture device. 

More than one video capture context can be created but they must be
connected to different devices. If the context is connected it will
disconnect first. Signals CONNECTION-ERROR on error."
  (check-type device (integer 0 *))
  (let ((result (im-capture-cffi::%im-video-capture-connect context device)))
    (when (zerop result)
	(error 'connection-error))))

(defun connected-device (context)
  "Returns the device number connected to an video capture context or
nil if it is not connected."
  (let ((connected-device (im-capture-cffi::%im-video-capture-connect context -1)))
    (if (= connected-device -1)
	nil
	connected-device)))

(defalias disconnect #'im-capture-cffi::%im-video-capture-disconnect
  "Disconnect from a capture device context.")

(defalias dialog-count #'im-capture-cffi::%im-video-capture-dialog-count
  "Returns the number of available configuration dialogs.")

(defun show-dialog (context dialog &optional (parent (cffi:null-pointer)))
  "Displays a configuration modal dialog of the connected device

In windows, the capturing will be stopped in some cases. in windows,
parent is a hwnd of a parent window. DIALOG can be from 0 to
DIALOG-COUNT.

Signals device-configuration-error on error."
  (im-capture-cffi::%im-video-capture-show-dialog context dialog parent))

(defalias dialog-description #'im-capture-cffi::%im-video-capture-dialog-desc
  "Returns the description of a configuration dialog. DIALOG can be
from 0 to DIALOG-COUNT.")

(defun set-in-out (context input output &optional (cross-index 1))
  "Allows to control the input and output of devices that have
multiple input and outputs.
 
The cross index controls in which stage the input/output will be
set. It is usually use 1, but some capture boards has a second
stage. In DirectX it controls the cross-bars.

Signals DEVICE-CONFIGURATION-ERROR on error."
  (when (im-capture-cffi::%im-video-capture-set-in-out
	 context
	 input
	 output
	 cross-index)
    (error 'device-configuration-error)))

(defun format-count (context)
  "Returns the number of available video formats. Signals
DEVICE-CONFIGURATION-ERROR on error."
  (let ((count (im-capture-cffi::%im-video-capture-format-count context)))
    (if (zerop count)
	(error 'device-configuration-error)
	count)))

(defun format (context format)
  "Returns information about the video format as values WIDTH HEIGHT
DESCRIPTION. Signals DEVICE-CONFIGURATION-ERROR on error."
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
  "Changes the video format of the connected device. 

Should not work for DV devices. Use (SETF IMAGE-SIZE) only. When the
format is changed in the dialog, for some formats the returned format
is the preferred format, not the current format. This will not affect
the COLOR-MODE-CONFIGURATION or COLOR-SPACE of the captured image.

Signals DEVICE-CONFIGURATION-ERROR on error."
  (check-type format (integer 0 *))
  (if (zerop (im-capture-cffi::%im-video-capture-set-format context format))
      (error 'device-configuration-error)
      format))

(defun current-format (context)
  "Returns the current format index. Signals
DEVICE-CONFIGURATION-ERROR on error."
  (let ((result (im-capture-cffi::%im-video-capture-set-format context -1)))
    (if (= result -1)
	(error 'device-configuration-error)
	result)))

(defun image-size (context)
  "Returns the current image size of the connected device as width and
height values. Signals DEVICE-CONFIGURATION-ERROR on error."
  (cffi:with-foreign-objects
      ((width-ptr :int)
       (height-ptr :int))
    (im-capture-cffi::%im-video-capture-get-image-size context width-ptr height-ptr)
    (let ((result (cffi:mem-ref width-ptr :int)))
      (if (zerop result)
	  (error 'device-configuration-error)
	  (values (cffi:mem-ref width-ptr :int)
		  (cffi:mem-ref height-ptr :int))))))

;; TODO int im_decl imvideocapturesetimagesize(imvideocapture* vc, int width, int height);

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
  "Returns a new captured frame. 

TIMEOUT is NIL for infinite timeout.  COLOR-SPACE can be
:COLOR-SPACE-RGB or :COLOR-SPACE-GRAY, and COLOR-MODE-CONFIG can be
:COLOR-MODE-CONFIG-PACKED or not. Data type is always
:DATA-TYPE-BYTE. It can not have an alpha channel and orientation is
always bottom up.

DATA is a static vector (see STATIC-VECTORS:MAKE-STATIC-VECTOR) with
length greater than or equal to computed image data size, and must
have element type (UNSIGNED-BYTE 8).

Use IM:IMAGE-DATA_SIZE to compute the length of DATA, using
IM-CAPTURE:IMAGE-SIZE to compute WIDTH and HEIGHT. e.g.

    (multiple-value-bind
          (width height)
        (im-capture:image-size context)
        (im:image-data-size
         width height
         '(:COLOR-MODE-CONFIG-PACKED) :COLOR-SPACE-RGB :DATA-TYPE-BYTE))

Signals CAPTURE-ERROR on error or timeout expired and the DATA buffer
remains unchanged."
  (%verify-capture-frame-data-vector context data color-mode-config color-space)
  (when (zerop (im-capture-cffi::%im-video-capture-frame
		context
		(static-vectors:static-vector-pointer data)
		(im::%encode-color-mode color-mode-config color-space)
		(if timeout timeout -1)))
    (error 'capture-error)))

(defun capture-one-frame (context data color-mode-config color-space)
  "Start capturing, fill DATA with the new captured frame and 
capturing.

This is more useful if you are switching between devices. The data
format is the same as CAPTURE-FRAME. Signals CAPTURE-ERROR on error."
  (%verify-capture-frame-data-vector context data color-mode-config color-space)
  (when (zerop (im-capture-cffi::%im-video-capture-one-frame
	       context
	       (static-vectors:static-vector-pointer data)
	       (im::%encode-color-mode color-mode-config color-space)))
    (error 'capture-error)))

(defun live (context live-p)
  "Start/stop capturing according to LIVE-P. Signals CAPTURE-ERROR on
error."
  (let ((result (im-capture-cffi::%im-video-capture-live context (if live-p 1 0))))
    (if (zerop result)
	(error 'capture-error))))

(defun live-p (context)
  "Returns T if the context is live (capturing), NIL if not live."
  (not (zerop (im-capture-cffi::%im-video-capture-live context -1))))


