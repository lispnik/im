(defsystem #:im-all
  :author "Matthew Kennedy <burnsidemk@gmail.com>"
  :homepage "https://github.com/lispnik/im"
  :licence "MIT"
  :depends-on (#:im
               #+windows #:im-avi
               #+windows #:im-capture
	       #:im-jp2
	       #:im-process
	       #+windows #:im-wmv))
