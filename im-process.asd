(defsystem #:im-process
  :serial t
  :pathname "process"
  :components ((:file "process")
	       (:file "statistics")
	       (:file "morph"))
  :depends-on (#:im
               #:im-process-cffi))
