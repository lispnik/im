(defsystem #:im-process
  :serial t
  :pathname "process"
  :components ((:file "process")
	       (:file "statistics")
               (:file "arithmetic")
               (:file "convolve")
               (:file "threshold")
               (:file "color")
	       (:file "morph")
               (:file "geometric")
	       (:file "synthetic")
               (:file "analyze")
               (:file "transform"))
  :depends-on (#:im
               #:im-process-cffi))
