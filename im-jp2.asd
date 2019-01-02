(defsystem #:im-jp2
  :serial t
  :pathname "jp2"
  :components ((:file "jp2"))
  :depends-on (#:im
               #:im-jp2-cffi))
