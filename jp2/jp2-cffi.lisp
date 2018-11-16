(defpackage #:im-jp2-cffi
  (:use #:common-lisp))

(in-package #:im-jp2-cffi)

(cffi:define-foreign-library lib-im-jp2
  (:unix "libim_jp2.so")
  (:windows "im_jp2.dll")
  (t (:default "im_jp")))

(cffi:use-foreign-library lib-im-jp2)

(cffi:defcfun (%im-format-register-jp2 "imFormatRegisterJP2") :void)
