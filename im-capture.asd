(defsystem #:im-capture
  :serial t
  :pathname "capture"
  :components ((:file "capture"))
  :depends-on (#:im-capture-cffi
	       #:im
               #:cffi
	       #:static-vectors
               #:alexandria))
