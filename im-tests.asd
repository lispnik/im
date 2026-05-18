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
                :components ((:file "package")
                            (:file "info")
                            ;; (:file "processing")  ; TODO: Fix API calls
                            ;; (:file "analysis")    ; TODO: Fix API calls
                            )))
  :depends-on (#:fiveam
               #:im
               #:im-process)
  :perform (test-op (op c)
                    (uiop:symbol-call :fiveam :run! (uiop:find-symbol* :im-suite :im-tests))))
