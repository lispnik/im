(defpackage #:im-jp2
  (:use #:common-lisp
	#:cffi)
  (:import-from #:tecgraf-base #:defalias)
  (:export #:format-register-jp2))

(in-package #:im-jp2)

(defalias format-register-jp2 #'im-jp2-cffi::%im-format-register-jp2)

