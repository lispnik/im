(defpackage #:im-capture
  (:use #:common-lisp
	#:cffi
	#:serapeum)
  (:export #:invalid-device-error
	   #:no-device-error
	   #:connection-error
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
	   #:disconnect)
  (:shadow #:format))

(in-package #:im-capture)

(define-condition invalid-device-error () ())
(define-condition no-device-error () ())
(define-condition connection-error () ())
(define-condition device-configuration-error () ())

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
  "Returns the device path configuration. This is a unique
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
Windows the DirectX version is older than 8."
  (let ((context (im-capture-cffi::%im-video-capture-create)))
    (if (cffi:null-pointer-p context)
	(error 'no-device-error)
	context)))

(defun destroy (context)
  "Destroys a video capture context returned by CREATE."
  (im-capture-cffi::%im-video-capture-destroy context))

(defun connect (context device)
  "Connects to a capture device. 

More than one video capture context can be created but they must be
connected to different devices. If the context is connected it will
disconnect first."
  (check-type device (integer 0 *))
  (let ((result (im-capture-cffi::%im-video-capture-connect context device)))
    (when (zerop result)
	(error 'connection-error))))

(defun connected-device (context)
  "Returns the device number connected to an video capture context or
NIL if it is not connected."
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

In Windows, the capturing will be stopped in some cases. In Windows,
PARENT is a HWND of a parent window. DIALOG can be from 0 to
DIALOG-COUNT.

Signals DEVICE-CONFIGURATION-ERROR on error."
  (im-capture-cffi::%im-video-capture-show-dialog context dialog parent))

(defalias dialog-description #'im-capture-cffi::%im-video-capture-dialog-desc
  "Returns the description of a configuration dialog. DIALOG can be
from 0 to DIALOG-COUNT.")

(defun set-in-out (context input output &optional (cross-index 1))
  "Allows to control the input and output of devices that have
multiple input and outputs.
 
The cross index controls in which stage the input/output will be
set. Usually use 1, but some capture boards has a second stage. In
Direct X it controls the cross-bars. 

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
and DESCRIPTION. Signals DEVICE-CONFIGURATION-ERROR on error."
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

Should NOT work for DV devices. Use (SETF IMAGE-SIZE) only. When the
format is changed in the dialog, for some formats the returned format
is the preferred format, not the current format. This will not affect
the color-mode-configuration or color-space of the captured image.

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
