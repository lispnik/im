(defpackage #:im-wmv-cffi
  (:use #:common-lisp))

(in-package #:im-wmv-cffi)

(cffi:define-foreign-library lib-im-wmv
  (:windows "im_wmv.dll")
  (t (:default "im_wmv")))

(cffi:use-foreign-library lib-im-wmv)

;;; im_format_wmv.h

(cffi:defcfun (%im-format-register-wmv "imFormatRegisterWMV") :void)
