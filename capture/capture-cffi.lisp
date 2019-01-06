(defpackage #:im-capture-cffi
  (:use #:common-lisp))

(in-package #:im-capture-cffi)

(cffi:define-foreign-library lib-im-capture
  (:windows "im_capture.dll")
  (t (:default "im_capture")))

(cffi:use-foreign-library lib-im-capture)

;;; im_capture.h

(cffi:defctype im-video-capture :pointer)

(cffi:defcfun (%im-video-capture-device-count "imVideoCaptureDeviceCount") :int)

(cffi:defcfun (%im-video-capture-device-desc "imVideoCaptureDeviceDesc") :string
  (device :int))

(cffi:defcfun (%im-video-capture-device-ex-desc "imVideoCaptureDeviceExDesc") :string
  (device :int))

(cffi:defcfun (%im-video-capture-device-path "imVideoCaptureDevicePath") :string
  (device :int))

(cffi:defcfun (%im-video-capture-device-vendor-info "imVideoCaptureDeviceVendorInfo") :string
  (device :int))

(cffi:defcfun (%im-video-capture-reload-devices "imVideoCaptureReloadDevices") :int)
(cffi:defcfun (%im-video-capture-release-devices "imVideoCaptureReleaseDevices") :void)
(cffi:defcfun (%im-video-capture-create "imVideoCaptureCreate") im-video-capture)

(cffi:defcfun (%im-video-capture-create "imVideoCaptureDestroy") :void
  (vc im-video-capture))

(cffi:defcfun (%im-video-capture-connect "imVideoCaptureConnect") :int
  (vc im-video-capture)
  (device :int))

(cffi:defcfun (%im-video-capture-disconnect "imVideoCaptureDisconnect") :void
  (vc im-video-capture))

(cffi:defcfun (%im-video-capture-dialog-count "imVideoCaptureDialogCount") :int
  (vc im-video-capture))

(cffi:defcfun (%im-video-capture-show-dialog "imVideoCaptureShowDialog") :int
  (vc im-video-capture)
  (dialog :int)
  (parent :pointer))

(cffi:defcfun (%im-video-capture-dialog-desc "imVideoCaptureDialogDesc") :string
  (vc im-video-capture)
  (dialog :int))

(cffi:defcfun (%im-video-capture-set-in-out "imVideoCaptureSetInOut") :int
  (vc im-video-capture)
  (input :int)
  (output :int)
  (cross :int))

(cffi:defcfun (%im-video-capture-format-count "imVideoCaptureFormatCount") :int
  (vc im-video-capture))

(cffi:defcfun (%im-video-capture-get-format "imVideoCaptureGetFormat") :int
  (vc im-video-capture)
  (format :int)
  (width (:pointer :int))
  (height (:pointer :int))
  (desc :pointer))			;a pointer to 10 bytes

(cffi:defcfun (%im-video-capture-set-format "imVideoCaptureSetFormat") :int
  (vc im-video-capture)
  (format :int))

(cffi:defcfun (%im-video-capture-get-image-size "imVideoCaptureGetImageSize") :void
  (vc im-video-capture)
  (width (:pointer :int))
  (height (:pointer :int)))

(cffi:defcfun (%im-video-capture-set-image-size "imVideoCaptureSetImageSize") :int
  (vc im-video-capture)
  (width :int)
  (height :int))

(cffi:defcfun (%im-video-capture-frame "imVideoCaptureFrame") :int
  (vc im-video-capture)
  (data :pointer)
  (color-mode :int)
  (time-out :int))

(cffi:defcfun (%im-video-capture-one-frame "imVideoCaptureOneFrame") :int
  (vc im-video-capture)
  (data :pointer)
  (color-mode :int))

(cffi:defcfun (%im-video-capture-live "imVideoCaptureLive") :int
  (vc im-video-capture)
  (live :int))

(cffi:defcfun (%im-video-capture-reset-attribute "imVideoCaptureResetAttribute") :int
  (vc im-video-capture)
  (fauto :boolean))

(cffi:defcfun (%im-video-capture-get-attribute "imVideoCaptureGetAttribute") :int
  (vc im-video-capture)
  (attrib :string)
  (percent (:pointer :double)))

(cffi:defcfun (%im-video-capture-set-attribute "imVideoCaptureSetAttribute") :int
  (vc im-video-capture)
  (attrib :string)
  (percent :double))

(cffi:defcfun (%im-video-capture-get-attribute-list "imVideoCaptureGetAttributeList") :pointer
  (vc im-video-capture)
  (num-attrib (:pointer :int)))
