(defpackage #:im-avi
  (:use #:common-lisp
	#:cffi)
  (:import-from #:tecgraf-base #:defalias)
  (:export #:format-register-avi))

(in-package #:im-avi)

(defalias format-register-avi #'im-avi-cffi::%im-format-register-avi)

