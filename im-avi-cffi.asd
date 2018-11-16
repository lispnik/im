#-windows (error "AVI is supported on Windows only")

(defsystem #:im-avi-cffi
  :serial t
  :pathname "avi"
  :components ((:file "avi-cffi"))
  :depends-on (#:im-cffi
               #:cffi))
