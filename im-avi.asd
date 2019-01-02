(defsystem #:im-avi
  :serial t
  :pathname "avi"
  :components ((:file "avi"))
  :depends-on (#:im
               #:im-avi-cffi))
