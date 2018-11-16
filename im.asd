(defsystem #:im
  :description "CFFI bindings to IM, a toolkit for image representation, storage, capture and processing"
  :author "Matthew Kennedy <burnsidemk@gmail.com>"
  :homepage "https://github.com/lispnik/im"
  :licence "MIT"
  :version (:read-file-line "version.txt")
  :serial t
  :pathname "im"
  :components ((:file "package")
               (:file "version")
               (:file "error")
	       (:file "im"))
  :depends-on (#:im-cffi
               #:cffi
               #:alexandria
               #:serapeum))
