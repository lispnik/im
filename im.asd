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
               (:file "util")
               (:file "file")
               (:file "format")
               (:file "palette")
               (:file "convert")
               (:file "image")
               (:file "compression"))
  :depends-on (#:im-cffi
               #:cffi
               #:alexandria
               #:split-sequence
               #:tecgraf-base))
