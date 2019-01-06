(defsystem #:im-avi
  :serial t
  :pathname "avi"
  :components ((:file "avi"))
  :depends-on (#:im
               #:im-avi-cffi)
  :perform (load-op :after (o c) (uiop:symbol-call "IM-AVI" "FORMAT-REGISTER-AVI")))
