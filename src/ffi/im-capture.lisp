;;;; src/ffi/im-capture.lisp — DRAFTED by tools/gen-bindings.lisp.
;;;;
;;;; Source: im_capture.h
;;;; Hand corrections below this line are expected and are kept;
;;;; re-run the generator into a clean tree and diff.

(in-package #:im.ffi)

(cffi:defcfun ("imVideoCaptureDeviceCount" %im-video-capture-device-count) :int
  "Returns the number of available devices.")

(cffi:defcfun ("imVideoCaptureDeviceDesc" %im-video-capture-device-desc) :string
  "Returns the device description. Returns NULL only if it is an invalid
device."
  (device :int))

(cffi:defcfun ("imVideoCaptureDeviceExDesc" %im-video-capture-device-ex-desc) :string
  "Returns the extended device description. May return NULL."
  (device :int))

(cffi:defcfun ("imVideoCaptureDevicePath" %im-video-capture-device-path) :string
  "Returns the device path configuration. This is a unique string."
  (device :int))

(cffi:defcfun ("imVideoCaptureDeviceVendorInfo" %im-video-capture-device-vendor-info) :string
  "Returns the vendor information. May return NULL."
  (device :int))

(cffi:defcfun ("imVideoCaptureReloadDevices" %im-video-capture-reload-devices) :int
  "Reload the device list. The devices can be dynamically removed or added to
the system. Returns the number of available devices.")

(cffi:defcfun ("imVideoCaptureReleaseDevices" %im-video-capture-release-devices) :void
  "Release the device list. Useful is you need to track leak erros in your
application.")

(cffi:defcfun ("imVideoCaptureCreate" %im-video-capture-create) im-video-capture
  "Creates a new imVideoCapture object. Returns NULL if there is no capture
device available. In Windows returns NULL if DirectX version is older than
8. In Lua the IM videocapture metatable name is \"imVideoCapture\". When
converted to a string will return \"imVideoCapture(%p)\" where %p is
replaced by the userdata address. If the videocapture is already destroyed
by im.VideoCaptureDestroy, then it will return also the suffix
\"-destroyed\".")

(cffi:defcfun ("imVideoCaptureDestroy" %im-video-capture-destroy) :void
  "Destroys a imVideoCapture object. In Lua if this function is not called,
the videocapture is destroyed by the garbage collector."
  (vc im-video-capture))

(cffi:defcfun ("imVideoCaptureConnect" %im-video-capture-connect) :int
  "Connects to a capture device. More than one imVideoCapture object can be
created but they must be connected to different devices. If the object is
connected it will disconnect first. Use -1 to return the current connected
device, in this case returns -1 if not connected. Returns zero if failed."
  (vc im-video-capture)
  (device :int))

(cffi:defcfun ("imVideoCaptureDisconnect" %im-video-capture-disconnect) :void
  "Disconnect from a capture device."
  (vc im-video-capture))

(cffi:defcfun ("imVideoCaptureDialogCount" %im-video-capture-dialog-count) :int
  "Returns the number of available configuration dialogs."
  (vc im-video-capture))

(cffi:defcfun ("imVideoCaptureShowDialog" %im-video-capture-show-dialog) :int
  "Displays a configuration modal dialog of the connected device. In Windows,
the capturing will be stopped in some cases. In Windows parent is a HWND
of a parent window, it can be NULL. dialog can be from 0 to
imVideoCaptureDialogCount. Returns zero if failed."
  (vc im-video-capture)
  (dialog :int)
  (parent :pointer))

(cffi:defcfun ("imVideoCaptureDialogDesc" %im-video-capture-dialog-desc) :string
  "Returns the description of a configuration dialog. dialog can be from 0 to
imVideoCaptureDialogCount."
  (vc im-video-capture)
  (dialog :int))

(cffi:defcfun ("imVideoCaptureSetInOut" %im-video-capture-set-in-out) :int
  "Allows to control the input and output of devices that have multiple input
and outputs. The cross index controls in which stage the input/output will
be set. Usually use 1, but some capture boards has a second stage. In
Direct X it controls the crossbars."
  (vc im-video-capture)
  (input :int)
  (output :int)
  (cross :int))

(cffi:defcfun ("imVideoCaptureFormatCount" %im-video-capture-format-count) :int
  "Returns the number of available video formats. Returns zero if failed."
  (vc im-video-capture))

(cffi:defcfun ("imVideoCaptureGetFormat" %im-video-capture-get-format) :int
  "Returns information about the video format. format can be from 0 to
imVideoCaptureFormatCount. desc should be of size 10. The image size is
usually the maximum size for that format. Other sizes can be available
using imVideoCaptureSetImageSize. Returns zero if failed."
  (vc im-video-capture)
  (format :int)
  (width :pointer)
  (height :pointer)
  (desc :pointer))

(cffi:defcfun ("imVideoCaptureSetFormat" %im-video-capture-set-format) :int
  "Changes the video format of the connected device. Should NOT work for DV
devices. Use imVideoCaptureSetImageSize only. Use -1 to return the current
format, in this case returns -1 if failed. When the format is changed in
the dialog, for some formats the returned format is the preferred format,
not the current format. This will not affect color_mode of the capture
image. Returns zero if failed."
  (vc im-video-capture)
  (format :int))

(cffi:defcfun ("imVideoCaptureGetImageSize" %im-video-capture-get-image-size) :void
  "Returns the current image size of the connected device. width and height
returns 0 if not connected."
  (vc im-video-capture)
  (width :pointer)
  (height :pointer))

(cffi:defcfun ("imVideoCaptureSetImageSize" %im-video-capture-set-image-size) :int
  "Changes the image size of the connected device. Similar to
imVideoCaptureSetFormat, but changes only the size. Valid sizes can be
obtained with imVideoCaptureGetFormat. Returns zero if failed."
  (vc im-video-capture)
  (width :int)
  (height :int))

(cffi:defcfun ("imVideoCaptureFrame" %im-video-capture-frame) :int
  "Returns a new captured frame. Use -1 for infinite timeout. Color space can
be IM_RGB or IM_GRAY, and mode can be packed (IM_PACKED) or not. Data type
is always IM_BYTE. It can not have an alpha channel and orientation is
always bottom up. Returns zero if failed or timeout expired, the buffer is
not changed."
  (vc im-video-capture)
  (data :pointer)
  (color-mode :int)
  (timeout :int))

(cffi:defcfun ("imVideoCaptureOneFrame" %im-video-capture-one-frame) :int
  "Start capturing, returns the new captured frame and stop capturing. This
is more useful if you are switching between devices. Data format is the
same as imVideoCaptureFrame. Returns zero if failed."
  (vc im-video-capture)
  (data :pointer)
  (color-mode :int))

(cffi:defcfun ("imVideoCaptureLive" %im-video-capture-live) :int
  "Start capturing. Use -1 to return the current state. Returns zero if
failed."
  (vc im-video-capture)
  (live :int))

(cffi:defcfun ("imVideoCaptureResetAttribute" %im-video-capture-reset-attribute) :int
  "Resets a camera or video attribute to the default value or to the
automatic setting. Not all attributes support automatic modes. Returns
zero if failed."
  (vc im-video-capture)
  (attrib :string)
  (fauto :int))

(cffi:defcfun ("imVideoCaptureGetAttribute" %im-video-capture-get-attribute) :int
  "Returns a camera or video attribute in percentage of the valid range
value. Returns zero if failed or attribute not supported."
  (vc im-video-capture)
  (attrib :string)
  (percent :pointer))

(cffi:defcfun ("imVideoCaptureSetAttribute" %im-video-capture-set-attribute) :int
  "Changes a camera or video attribute in percentage of the valid range
value. Returns zero if failed or attribute not supported."
  (vc im-video-capture)
  (attrib :string)
  (percent :double))

(cffi:defcfun ("imVideoCaptureGetAttributeList" %im-video-capture-get-attribute-list) :pointer
  "Returns a list of the description of the valid attributes for the device
class. But each device may still not support some of the returned
attributes. Use the return value of imVideoCaptureGetAttribute to check if
the attribute is supported."
  (vc im-video-capture)
  (num-attrib :pointer))
