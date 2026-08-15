(defsystem #:im-tests
  :description "FiveAM integration tests for the high-level IM CL APIs."
  :author "Matthew Kennedy <burnsidemk@gmail.com>"
  :licence "MIT"
  :serial t
  :pathname "tests"
  :components ((:file "package")
               (:file "fixtures")
               (:file "image")
               (:file "file")
               (:file "palette")
               (:file "counter")
               (:file "binfile")
               (:file "arithmetic")
               (:file "convolve")
               (:file "threshold")
               (:file "color")
               (:file "morph")
               (:file "geometric")
               (:file "render")
               (:file "analyze")
               (:file "transform")
               (:file "calc")
               (:module "examples"
                :serial t
                :components ((:file "package")
                             (:file "info")
                             (:file "processing")
                             (:file "analysis"))))
  :depends-on (#:fiveam
               #:im
               #:im-process)
  ;; FiveAM's RUN! prints the report and returns NIL when anything
  ;; failed. Discarding that return value makes TEST-OP succeed on a
  ;; failing suite, which is how CI went green with 3 tests failing.
  :perform (test-op (op c)
                    (unless (uiop:symbol-call :fiveam :run! (uiop:find-symbol* :im-suite :im-tests))
                      (error "IM test suite failed."))))
