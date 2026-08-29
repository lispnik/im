;;;; tests/package.lisp — suite definition and shared helpers.

(defpackage #:im.tests
  (:use #:common-lisp #:fiveam)
  (:shadowing-import-from #:fiveam #:test)
  (:export #:im-suite #:run-all))

(in-package #:im.tests)

(def-suite im-suite
  :description "The IM Common Lisp bindings.")

(defun run-all ()
  "Run the whole suite. Returns NIL if anything failed."
  (run! 'im-suite))

;;; Helpers -------------------------------------------------------------------

(defparameter *image-dir*
  (asdf:system-relative-pathname "im" "tests/images/")
  "Sample images, resolved through ASDF so the suite runs from any directory.")

(defun image-file (name)
  (merge-pathnames name *image-dir*))

(defparameter *tmp-dir*
  (uiop:ensure-directory-pathname
   (uiop:merge-pathnames* "im-tests/" (uiop:temporary-directory))))

(defun tmp-file (name)
  (ensure-directories-exist *tmp-dir*)
  (uiop:merge-pathnames* name *tmp-dir*))

(defun gray-gradient (width height)
  "A gray byte image whose samples ascend, so any resample or filter shows."
  (let* ((image (im:create width height :color-space-gray :data-type-byte))
         (plane (im:plane-pointer image 0)))
    (dotimes (i (* width height) image)
      (setf (cffi:mem-aref plane :unsigned-char i) (logand i #xff)))))

(defun binary-block (&key (width 32) (height 32) (x 8) (y 8) (size 8))
  "A binary image containing one solid SIZE by SIZE block."
  (let* ((image (im:create width height :color-space-binary :data-type-byte))
         (plane (im:plane-pointer image 0)))
    (dotimes (i (* width height)) (setf (cffi:mem-aref plane :unsigned-char i) 0))
    (loop for row from y below (+ y size)
          do (loop for col from x below (+ x size)
                   do (setf (cffi:mem-aref plane :unsigned-char (+ (* row width) col)) 1)))
    image))

(defun pixel (image plane x y)
  (cffi:mem-aref (im:plane-pointer image plane) :unsigned-char
                 (+ (* y (im:width image)) x)))

(defun set-pixels (image plane value)
  (dotimes (i (im:pixel-count image))
    (setf (cffi:mem-aref (im:plane-pointer image plane) :unsigned-char i) value)))
