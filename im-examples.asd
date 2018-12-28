(defsystem #:im-examples
  :serial t
  :pathname "examples"
  :components ((:file "image-info")
               (:file "image-copy"))
  :depends-on (#:im
               #+windows #:im-avi
               #+windows #:im-wmv))
