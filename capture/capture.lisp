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
	   #:disconnect))

(in-package #:im-capture)

(define-condition invalid-device-error () ())
(define-condition no-device-error () ())
(define-condition connection-error () ())

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
	(error 'no-device-error))))

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
