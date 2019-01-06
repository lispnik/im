(defsystem #:im-wmv
  :serial t
  :pathname "wmv"
  :components ((:file "wmv"))
  :depends-on (#:im
               #:im-wmv-cffi)
  :perform (load-op :after (o c) (uiop:symbol-call "IM-WMV" "FORMAT-REGISTER-WMV")))
