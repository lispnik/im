(defsystem #:im-examples
  :serial t
  :pathname "examples"
  :components ((:file "image-info")
               (:file "image-copy")
               #+windows (:file "cheese"))
  :depends-on (#:im
               #:static-vectors
               #+windows #:im-avi
               #+windows #:im-wmv
               #+windows #:im-capture))
