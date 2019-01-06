#-windows (error "WMV is supported on Windows only")

(defsystem #:im-wmv-cffi
  :serial t
  :pathname "wmv"
  :components ((:file "wmv-cffi"))
  :depends-on (#:im-cffi
               #:cffi))
