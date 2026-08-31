;;;; tests/display.lisp — DISPLAY and its front-end dispatch.
;;;;
;;;; The two real backends need an Emacs on the other end, so what is testable
;;;; here is everything around them: that the file DISPLAY hands over is a
;;;; readable image, that a front end which declines is reported rather than
;;;; ignored, and that the temporary directory does not grow without bound.
;;;;
;;;; Every test binds *DISPLAY-FUNCTION*, which short-circuits the SLY and
;;;; SLIME probes. Without that, running this suite from inside a SLY session
;;;; would pop image buffers at the person running it and fail besides.

(in-package #:im.tests)

(def-suite display-suite :in im-suite
  :description "Displaying an image in a REPL front end.")
(in-suite display-suite)

(defmacro with-display-directory (&body body)
  "Run BODY with DISPLAY writing into an empty directory of its own.

IM:*DISPLAY-DIRECTORY* is where it writes, and what a test that needs the
files reads."
  `(let ((directory (uiop:ensure-directory-pathname (tmp-file "display/"))))
     (uiop:delete-directory-tree directory :validate t :if-does-not-exist :ignore)
     (let ((im:*display-directory* directory))
       ,@body)))

(test display-writes-a-readable-image-and-names-the-backend
  (with-display-directory
    (let (seen)
      (let ((im:*display-function* (lambda (image pathname)
                                     (declare (ignore image))
                                     (setf seen pathname)
                                     :test-harness)))
        (im:with-image (image (gray-gradient 8 8))
          (multiple-value-bind (returned backend) (im:display image)
            (is (eq image returned) "DISPLAY returns the image it was given")
            (is (eq :test-harness backend))))
        (is-true (probe-file seen))
        (is (string= "PNG" (getf (im:file-info seen) :format)))))))

(test display-honours-an-explicit-pathname
  (let ((path (tmp-file "explicit.png")))
    (when (probe-file path) (delete-file path))
    (let ((im:*display-function* (lambda (image pathname)
                                   (declare (ignore image pathname))
                                   :test-harness)))
      (im:with-image (image (gray-gradient 4 4))
        (im:display image :pathname path))
      (is-true (probe-file path)))))

(test display-reports-a-front-end-that-cannot-show-the-image
  "A NIL from *DISPLAY-FUNCTION* is a refusal, not a success.

The same path a bare REPL takes, where no backend claims the image. Returning
normally there would leave the caller believing an image is on screen."
  (with-display-directory
    (let ((im:*display-function* (lambda (image pathname)
                                   (declare (ignore image pathname))
                                   nil)))
      (im:with-image (image (gray-gradient 4 4))
        (signals im:display-unavailable (im:display image))))))

(test display-keeps-its-temporary-directory-bounded
  "Old PNGs are deleted, but not the ones a front end may still be reading."
  (with-display-directory
    (let ((im:*display-function* (lambda (image pathname)
                                   (declare (ignore image pathname))
                                   :test-harness))
          (im:*display-history* 3))
      (im:with-image (image (gray-gradient 4 4))
        (dotimes (i 8) (im:display image)))
      (let ((files (directory (merge-pathnames "*.png" im:*display-directory*))))
        (is (<= (length files) 4)
            "~D files left behind with a history of 3" (length files))))))
