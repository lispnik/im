(defsystem #:im-process-cffi
  :serial t
  :pathname "process"
  :components ((:file "process-cffi"))
  :depends-on (#:im-cffi
               #:cffi))
