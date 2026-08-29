;;;; src/ffi/im-format-addon.lisp — DRAFTED by tools/gen-bindings.lisp.
;;;;
;;;; Source: im_format_jp2.h, im_format_heif.h, im_format_avi.h, im_format_wmv.h
;;;; Hand corrections below this line are expected and are kept;
;;;; re-run the generator into a clean tree and diff.

(in-package #:im.ffi)

(cffi:defcfun ("imFormatRegisterJP2" %im-format-register-jp2) :void
  "Register the JP2 Format. In Lua, when using require\"imlua_jp2\" this
function will be automatically called.")

(cffi:defcfun ("imFormatRegisterHEIF" %im-format-register-heif) :void
  "Register the HEIF and AVIF Formats. Registers two format drivers, \"HEIF\"
(*.heic;*.heif) and \"AVIF\" (*.avif), which share one implementation. In
Lua, when using require\"imlua_heif\" this function will be automatically
called.")
