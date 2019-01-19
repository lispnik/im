(defsystem #:im-cffi
  :serial t
  :pathname "im"
  :components ((:file "im-cffi"))
  :depends-on (#:cffi
               #:trivial-features
	       #:fpw))
