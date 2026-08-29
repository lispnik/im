;;;; src/capture.lisp — live video capture.
;;;;
;;;; Available on every platform, and useful on two of them. Upstream compiles
;;;; one backend per platform behind an identical 27-function interface:
;;;; AVFoundation on macOS, DirectShow on Windows, and a stub reporting zero
;;;; devices everywhere else. That is a deliberate choice on their part -- a
;;;; consumer can link it unconditionally and discover at runtime that there
;;;; are no cameras, rather than failing to build on Linux.
;;;;
;;;; So this file needs no feature conditionals. It needs the libim_capture
;;;; that the local IM was built with, which is not the default
;;;; (IM_BUILD_CAPTURE defaults to OFF), hence CAPTURE-AVAILABLE-P.

(in-package #:im)

(export '(capture-available-p
          device-count
          device-description
          device-path
          device-vendor-info
          devices
          reload-devices
          with-capture-device
          capture-image-size
          capture-frame))

(defun capture-available-p ()
  "True when libim_capture loaded, so the rest of this file will work."
  (library-loaded-p 'lib-im-capture))

(defun %require-capture ()
  (unless (capture-available-p)
    (cl:error 'capture-error
              :detail "libim_capture is not loaded; build IM with -DIM_BUILD_CAPTURE=ON")))

(defun device-count ()
  "The number of capture devices attached.

Zero is a normal answer, not a failure: on Linux upstream builds a stub
backend that always reports none."
  (%require-capture)
  (im.ffi::%im-video-capture-device-count))

(defun device-description (device)
  "A human-readable name for DEVICE, or NIL if the index is invalid."
  (%require-capture)
  (im.ffi::%im-video-capture-device-desc device))

(defun device-path (device)
  "The system path or identifier for DEVICE, or NIL."
  (%require-capture)
  (im.ffi::%im-video-capture-device-path device))

(defun device-vendor-info (device)
  "Vendor information for DEVICE, or NIL."
  (%require-capture)
  (im.ffi::%im-video-capture-device-vendor-info device))

(defun reload-devices ()
  "Re-enumerate devices after one has been plugged in or removed."
  (%require-capture)
  (im.ffi::%im-video-capture-reload-devices))

(defun devices ()
  "Every capture device, as a list of plists.

Each has :INDEX, :DESCRIPTION, :PATH and :VENDOR. Returns NIL when there are
none, which is the usual result on Linux."
  (%require-capture)
  (loop for index below (device-count)
        collect (list :index index
                      :description (device-description index)
                      :path (device-path index)
                      :vendor (device-vendor-info index))))

(defmacro with-capture-device ((var device) &body body)
  "Connect to DEVICE for the extent of BODY, disconnecting afterwards.

On macOS connecting requires NSCameraUsageDescription in an application
bundle. Without it the process is killed by TCC rather than being allowed to
fail, so a run from a plain terminal can die at this call with no condition to
catch. Enumerating devices is unaffected and always safe."
  (alexandria:with-gensyms (index handle)
    `(let* ((,index ,device)
            (,handle (progn
                       (%require-capture)
                       (let ((h (im.ffi::%im-video-capture-create)))
                         (when (cffi:null-pointer-p h)
                           (cl:error 'capture-error
                                     :detail "could not create a capture context"))
                         h))))
       (unwind-protect
            (progn
              (when (zerop (im.ffi::%im-video-capture-connect ,handle ,index))
                (cl:error 'device-connection-error
                          :detail (format nil "device ~D (~A)" ,index
                                          (or (device-description ,index) "unknown"))))
              (let ((,var ,handle))
                ,@body))
         (ignore-errors (im.ffi::%im-video-capture-disconnect ,handle))
         (im.ffi::%im-video-capture-destroy ,handle)))))

(defun capture-image-size (handle)
  "The (WIDTH . HEIGHT) the connected device is producing."
  (cffi:with-foreign-objects ((width :int) (height :int))
    (im.ffi::%im-video-capture-get-image-size handle width height)
    (cons (cffi:mem-ref width :int) (cffi:mem-ref height :int))))

(defun capture-frame (device &key (color-space :color-space-rgb))
  "Grab one frame from DEVICE and return it as an IMAGE.

The caller owns the image; use WITH-IMAGE or DESTROY."
  (with-capture-device (handle device)
    (destructuring-bind (width . height) (capture-image-size handle)
      (let ((image (create width height color-space :data-type-byte)))
        (handler-case
            (progn
              ;; imVideoCaptureOneFrame writes into the image's own buffer,
              ;; which is why plane 0 is handed over directly -- it addresses
              ;; the whole allocation, not just the first plane.
              (when (zerop (im.ffi::%im-video-capture-one-frame
                            handle (plane-pointer image 0)
                            (cffi:foreign-enum-value 'im.ffi::color-space color-space)))
                (cl:error 'capture-error
                          :detail (format nil "no frame from device ~D" device)))
              image)
          ;; The image is ours until it is returned; if anything goes wrong
          ;; between allocating it and handing it back, it leaks.
          (cl:error (c) (destroy image) (cl:error c)))))))
