(defpackage #:im
  (:use #:common-lisp
	#:cffi
	#:alexandria)
  (:import-from #:im-cffi #:im-image)
  (:import-from #:tecgraf-base #:defalias)
  (:export #:im-image)
  (:documentation "High-level Lisp API for IM-CFFI."))

(defpackage #:im-image
  (:use #:common-lisp
	#:cffi)
  (:import-from #:tecgraf-base #:defalias)
  (:shadow #:reshape))

(defpackage #:im-file
  (:use #:common-lisp
        #:alexandria
	#:cffi)
  (:shadow #:open
           #:close
           #:with-open-file)
  (:import-from #:im-cffi #:im-file #:im-image)
  (:import-from #:tecgraf-base #:defalias))

(defpackage #:im-palette
  (:use #:common-lisp
	#:cffi)
  (:shadow #:count
           #:sequence)
  (:import-from #:tecgraf-base #:defalias))
