(defsystem #:im-process
  :serial t
  :pathname "process"
  :components ((:file "process")
	       (:file "statistics"))
  :depends-on (#:im
               #:im-process-cffi))
