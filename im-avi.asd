(defsystem #:im-avi
  :serial t
  :pathname "avi"
  :components ((:file "package")
	       (:file "avi"))
  :depends-on (#:im
               #:im-avi-cffi))
