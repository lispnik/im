(defpackage #:im-tests
  (:use #:common-lisp #:fiveam)
  (:export #:im-suite
           #:run-all))

(in-package #:im-tests)

(def-suite im-suite
  :description "Integration tests for the high-level IM Common Lisp APIs.")

(defun run-all ()
  "Run the entire IM test suite. Returns NIL on success, signals on
failure when used in batch."
  (run! 'im-suite))
