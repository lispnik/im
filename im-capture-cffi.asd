#-windows (error "Capture is supported on Windows only")

(defsystem #:im-capture-cffi
  :serial t
  :pathname "capture"
  :components ((:file "capture-cffi"))
  :depends-on (#:cffi
               #:trivial-features))
