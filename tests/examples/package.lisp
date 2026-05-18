(in-package #:im-tests)

;;; IM Examples Test Suite
;;; Ported from Lua examples in tecgraf/im/html/examples/

(def-suite examples-suite
  :description "Tests ported from IM Lua examples"
  :in im-suite)

(defparameter *examples-image-dir*
  #p"/Users/mkennedy/Projects/common-lisp/im/tests/images/"
  "Directory containing test images for examples.")

(defun examples-image-path (filename)
  "Get full path to a test image file."
  (uiop:merge-pathnames* filename *examples-image-dir*))

(defun examples-image-exists-p (filename)
  "Check if a test image file exists."
  (uiop:file-exists-p (examples-image-path filename)))

;; Helper macros for example tests
(defmacro with-example-output ((var basename &optional format) &body body)
  "Create temporary output file for example test results."
  (let ((fmt (or format "png")))
    `(let ((,var (tmp-path (format nil "~a.~a" ,basename ,fmt))))
       ,@body)))

;; Test runner functions
(defun run-examples-suite ()
  "Run all ported Lua examples tests and return results."
  (format t "~%Running IM Examples Test Suite~%")
  (format t "============================~%~%")

  ;; Check that test images are available
  (format t "Checking test images...~%")
  (let ((missing-images '()))
    (dolist (image '("lena.jpg" "flower.gif" "flower.jpg" "rice.png"))
      (if (examples-image-exists-p image)
          (format t "✓ ~A~%" image)
          (progn
            (format t "✗ ~A (missing)~%" image)
            (push image missing-images))))

    (when missing-images
      (format t "~%Warning: Some test images are missing: ~{~A~^, ~}~%~%" missing-images)))

  (format t "~%Running test suites...~%")
  (let ((results (run! 'examples-suite)))
    (format t "~%Examples test suite completed.~%")
    results))

(defun run-examples-only ()
  "Run only the examples tests, skipping the main IM test suite."
  (run-examples-suite))

;; Add examples suite runner to the main test exports
(export 'run-examples-suite)
(export 'run-examples-only)