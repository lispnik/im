(defpackage #:im
  (:use #:common-lisp
	#:cffi
	#:alexandria
	#:serapeum)
  (:import-from #:im-cffi #:im-image)
  (:export #:im-image)
  (:documentation "High-level Lisp API for IM-CFFI."))
