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

(test display-without-a-front-end-signals-rather-than-crashing
  "The bare-REPL path, with no *DISPLAY-FUNCTION* to short-circuit the probes.

Every other test here binds that hook, which is what let an unguarded
FIND-SYMBOL through review: probing for a SWANK that was never loaded signals
a PACKAGE-ERROR, so DISPLAY crashed in exactly the situation
DISPLAY-UNAVAILABLE exists to report."
  (if (or (find-package '#:swank) (find-package '#:slynk))
      (skip "a REPL front end is loaded in this image")
      (with-display-directory
        (im:with-image (image (gray-gradient 4 4))
          (signals im:display-unavailable (im:display image))))))

(test display-backend-probes-tolerate-a-missing-package
  "FIND-SYMBOL signals on a package designator that names nothing; FIND-PACKAGE
does not. Every probe has to lead with the latter."
  (is (null (im::%attached-p '#:no-such-package-here)))
  (is (null (im::%loaded-symbol-function '#:no-such-package-here "ANYTHING")))
  (finishes (im::%swank-image-type "PNG")))

(test display-directory-without-a-trailing-slash-is-still-a-directory
  "#p\"/tmp/shots\" names a FILE to MERGE-PATHNAMES, so an unnormalised value
wrote the PNGs into the parent -- and pointed the history sweep at it."
  (let* ((directory (tmp-file "display-bare"))
         (written nil)
         (im:*display-directory* directory)
         (im:*display-function* (lambda (image pathname)
                                  (declare (ignore image))
                                  (setf written pathname)
                                  :test-harness)))
    (uiop:delete-directory-tree (uiop:ensure-directory-pathname directory)
                                :validate t :if-does-not-exist :ignore)
    (im:with-image (image (gray-gradient 4 4))
      (im:display image))
    (is (equal (pathname-directory (uiop:ensure-directory-pathname directory))
               (pathname-directory written))
        "wrote to ~A, outside the directory it was given" written)))

(test display-takes-the-format-from-an-explicit-pathname
  "Naming a file .tif and getting PNG bytes in it helps nobody."
  (let ((path (tmp-file "explicit-format.tif"))
        (im:*display-function* (lambda (image pathname)
                                 (declare (ignore image pathname))
                                 :test-harness)))
    (im:with-image (image (gray-gradient 4 4))
      (im:display image :pathname path)
      (is (string= "TIFF" (getf (im:file-info path) :format)))
      ;; ...and FORMAT still wins where the caller asks for it.
      (im:display image :pathname path :format "PNG")
      (is (string= "PNG" (getf (im:file-info path) :format))))))

(test lowering-the-history-sweeps-what-it-has-passed
  "Deleting one fixed offset per call stranded every file the offset had
already stepped past, permanently."
  (with-display-directory
    (let ((im:*display-function* (lambda (image pathname)
                                   (declare (ignore image pathname))
                                   :test-harness)))
      (im:with-image (image (gray-gradient 4 4))
        (let ((im:*display-history* 8))
          (dotimes (i 8) (im:display image)))
        (let ((im:*display-history* 2))
          (im:display image)))
      (let ((files (directory (merge-pathnames "*.png" im:*display-directory*))))
        (is (<= (length files) 3)
            "~D files left after lowering the history to 2" (length files))))))

(test the-sweep-leaves-alone-what-it-did-not-write
  "Pointing *DISPLAY-DIRECTORY* at your own directory must not cost you files.

*DISPLAY-COUNTER* restarts at zero each process, so the first call steps past
whatever numbered files are already there -- and the sweep, when it matched on
number alone, then reaped the ones it had stepped over. Twelve seeded files,
one DISPLAY call, five gone."
  (let* ((directory (uiop:ensure-directory-pathname (tmp-file "display-shared/")))
         (im:*display-directory* directory)
         (im:*display-function* (lambda (image pathname)
                                  (declare (ignore image pathname))
                                  :test-harness)))
    (uiop:delete-directory-tree directory :validate t :if-does-not-exist :ignore)
    (ensure-directories-exist directory)
    (let ((seeded (loop for i from 1 to 12
                        for path = (merge-pathnames (format nil "image-~D.png" i)
                                                    directory)
                        do (with-open-file (stream path :direction :output
                                                        :if-exists :supersede)
                             (write-string "not mine" stream))
                        collect path)))
      (im:with-image (image (gray-gradient 4 4))
        (dotimes (i 3) (im:display image)))
      (is (= 12 (count-if #'probe-file seeded))
          "~D of 12 files DISPLAY did not write survived"
          (count-if #'probe-file seeded)))))

(test the-format-decides-the-temporary-file-name-and-the-emacs-type
  "A temporary file named .png holding JPEG bytes renders as nothing in SLIME,
which reads the image type from the extension."
  (with-display-directory
    (let (written)
      (let ((im:*display-function* (lambda (image pathname)
                                     (declare (ignore image))
                                     (setf written pathname)
                                     :test-harness)))
        (im:with-image (image (gray-gradient 8 8))
          (im:display image :format "JPEG")))
      (is (string= "jpg" (pathname-type written)))
      (is (string= "JPEG" (getf (im:file-info written) :format)))
      ;; ...and that extension is what names the Emacs image type.
      (is (string= "JPEG" (cdr (assoc "jpg" im::*emacs-image-types*
                                      :test #'string=))))
      (is (null (cdr (assoc "jp2" im::*emacs-image-types* :test #'string=)))
          "a format Emacs has no type for must not be given one"))))
