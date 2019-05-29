(defpackage #:im
  (:use #:common-lisp
	#:cffi
	#:alexandria
	#:serapeum)
  (:import-from #:im-cffi #:im-image)
  (:export #:im-image)
  (:documentation "High-level Lisp API for IM-CFFI."))

(defpackage #:im-image
  (:use #:common-lisp
	#:cffi
	#:serapeum)
  (:shadow #:reshape))

(defpackage #:im-file
  (:use #:common-lisp
        #:alexandria
	#:cffi)
  (:shadow #:open
           #:close
           #:with-open-file)
  (:import-from #:im-cffi #:im-file #:im-image))

(defpackage #:im-palette
  (:use #:common-lisp
	#:cffi
	#:serapeum)
  (:shadow #:count
           #:sequence))
