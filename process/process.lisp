(defpackage #:im-process
  (:use #:common-lisp
	#:cffi
	#:serapeum)
  (:import-from #:im-process-cffi #:counter-aborted)
  (:export #:counter-aborted))

(in-package #:im-process)

