(defpackage #:im-process
  (:use #:common-lisp
	#:cffi
	#:serapeum)
  (:export #:counter-aborted))

(in-package #:im-process)

(define-condition counter-aborted () ())
