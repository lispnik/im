(defpackage #:im-wmv
  (:use #:common-lisp
	#:cffi
	#:serapeum)
  (:export #:format-register-wmv))

(in-package #:im-wmv)

(defalias format-register-wmv #'im-wmv-cffi::%im-format-register-wmv)
