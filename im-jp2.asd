(defsystem #:im-jp2
  :serial t
  :pathname "jp2"
  :components ((:file "jp2"))
  :depends-on (#:im
               #:im-jp2-cffi)
  :perform (load-op  :after (o c) (uiop:symbol-call "IM-JP2" "FORMAT-REGISTER-JP2")))
