(defpackage #:im-process
  (:use #:common-lisp
	#:cffi)
  (:import-from #:im-process-cffi #:counter-aborted)
  (:import-from #:tecgraf-base #:defalias)
  (:export #:counter-aborted))

(in-package #:im-process)
