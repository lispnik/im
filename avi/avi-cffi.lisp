(defpackage #:im-avi-cffi
  (:use #:common-lisp))

(in-package #:im-avi-cffi)

(cffi:define-foreign-library lib-im-avi
  (:windows "im_avi.dll")
  (t (:default "im_jp")))

(cffi:use-foreign-library lib-im-avi)

;;; im_format_avi.h

(cffi:defcfun (%im-format-register-avi "imFormatRegisterAVI") :void)
