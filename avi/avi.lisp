(defpackage #:im-avi
  (:use #:common-lisp
	#:cffi
	#:serapeum)
  (:export #:format-register-avi))

(in-package #:im-avi)

(defalias format-register-avi #'im-jp2-cffi::%im-format-register-avi)

