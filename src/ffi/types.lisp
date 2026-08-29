;;;; src/ffi/types.lisp — DRAFTED by tools/gen-bindings.lisp.
;;;;
;;;; IM's opaque handles. Each is a :POINTER underneath, but naming them
;;;; makes a signature say which kind of pointer it wants: imFileClose takes
;;;; an im-file and imImageDestroy an im-image, and mixing them up is then
;;;; visible in the source rather than at runtime.

(in-package #:im.ffi)

(cffi:defctype im-file :pointer)   ; imFile*
(cffi:defctype im-image :pointer)   ; imImage*
(cffi:defctype im-bin-file :pointer)   ; imBinFile*
(cffi:defctype im-video-capture :pointer)   ; imVideoCapture*
(cffi:defctype im-attrib-table :pointer)   ; imAttribTablePrivate*
