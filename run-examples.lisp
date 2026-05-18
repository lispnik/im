#!/usr/bin/env sbcl --script
;;;; Standalone runner for IM Lua examples ported to Common Lisp
;;;; Usage: sbcl --script run-examples.lisp
;;;; Or:    ./run-examples.lisp (if executable)

;; Helper function to exit from different Lisp implementations
(defun exit-lisp (&key (code 0))
  "Exit from Lisp with the given exit code."
  #+sbcl (sb-ext:exit :code code)
  #+ccl (ccl:quit code)
  #+clisp (ext:quit code)
  #+allegro (excl:exit code)
  #+cmu (unix:unix-exit code)
  #-(or sbcl ccl clisp allegro cmu)
  (error "Don't know how to exit on this Lisp implementation"))

(require :asdf)

;; Load the IM test system
(handler-case
    (progn
      (asdf:load-system :im-tests)
      (format t "✓ IM test system loaded successfully~%~%"))
  (error (e)
    (format t "✗ Failed to load IM test system: ~A~%" e)
    (format t "Make sure IM libraries are installed and accessible.~%")
    (exit-lisp :code 1)))

;; Run the examples test suite
(handler-case
    (progn
      (format t "Starting IM Examples Test Suite...~%")
      (let ((results (funcall (find-symbol "RUN-EXAMPLES-SUITE" "IM-TESTS"))))
        (if results
            (progn
              (format t "~%✓ All examples tests completed successfully!~%")
              (exit-lisp :code 0))
            (progn
              (format t "~%✗ Some examples tests failed.~%")
              (exit-lisp :code 1)))))
  (error (e)
    (format t "~%✗ Error running examples: ~A~%" e)
    (exit-lisp :code 1)))