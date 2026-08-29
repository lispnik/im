;;;; src/ffi/im-old.lisp — DRAFTED by tools/gen-bindings.lisp.
;;;;
;;;; Source: im_old.h
;;;; Hand corrections below this line are expected and are kept;
;;;; re-run the generator into a clean tree and diff.

(in-package #:im.ffi)

(cffi:defcfun ("imEncodeColor" %im-encode-color) :long
  (red :unsigned-char)
  (green :unsigned-char)
  (blue :unsigned-char))

(cffi:defcfun ("imDecodeColor" %im-decode-color) :void
  (red :pointer)
  (green :pointer)
  (blue :pointer)
  (palette :long))

(cffi:defcfun ("imFileFormat" %im-file-format) :int
  (filename :pointer)
  (format :pointer))

(cffi:defcfun ("imImageInfo" %im-image-info) :int
  (filename :pointer)
  (width :pointer)
  (height :pointer)
  (type :pointer)
  (palette-count :pointer))

(cffi:defcfun ("imLoadRGB" %im-load-rgb) :int
  (filename :pointer)
  (red :pointer)
  (green :pointer)
  (blue :pointer))

(cffi:defcfun ("imSaveRGB" %im-save-rgb) :int
  (width :int)
  (height :int)
  (format :int)
  (red :pointer)
  (green :pointer)
  (blue :pointer)
  (filename :pointer))

(cffi:defcfun ("imLoadMap" %im-load-map) :int
  (filename :pointer)
  (map :pointer)
  (palette :pointer))

(cffi:defcfun ("imSaveMap" %im-save-map) :int
  (width :int)
  (height :int)
  (format :int)
  (map :pointer)
  (palette-count :int)
  (palette :pointer)
  (filename :pointer))

(cffi:defcfun ("imRGB2Map" %im-rgb2-map) :void
  (width :int)
  (height :int)
  (red :pointer)
  (green :pointer)
  (blue :pointer)
  (map :pointer)
  (palette-count :int)
  (palette :pointer))

(cffi:defcfun ("imMap2RGB" %im-map2-rgb) :void
  (width :int)
  (height :int)
  (map :pointer)
  (palette-count :int)
  (colors :pointer)
  (red :pointer)
  (green :pointer)
  (blue :pointer))

(cffi:defcfun ("imRGB2Gray" %im-rgb2-gray) :void
  (width :int)
  (height :int)
  (red :pointer)
  (green :pointer)
  (blue :pointer)
  (map :pointer)
  (grays :pointer))

(cffi:defcfun ("imMap2Gray" %im-map2-gray) :void
  (width :int)
  (height :int)
  (map :pointer)
  (palette-count :int)
  (colors :pointer)
  (grey-map :pointer)
  (grays :pointer))

(cffi:defcfun ("imResize" %im-resize) :void
  (src-width :int)
  (src-height :int)
  (src-map :pointer)
  (dst-width :int)
  (dst-height :int)
  (dst-map :pointer))

(cffi:defcfun ("imStretch" %im-stretch) :void
  (src-width :int)
  (src-height :int)
  (src-map :pointer)
  (dst-width :int)
  (dst-height :int)
  (dst-map :pointer))

(cffi:defcfun ("imRegisterCallback" %im-register-callback) :int
  (cb :pointer)
  (cb-id :int)
  (format :int))
