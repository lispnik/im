(defsystem #:im-jp2-cffi
  :serial t
  :pathname "jp2"
  :components ((:file "jp2-cffi"))
  :depends-on (#:im-cffi
               #:cffi))
