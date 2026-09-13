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

(defun correlated-rgb (&key (width 48) (height 32))
  "An RGB byte image whose three planes lie close to one axis.

The shape a decorrelation stretch exists for -- pigment against rock -- and
the one where an operation that quietly did nothing could not be told from one
that worked. Deterministic, so a fit is reproducible between two calls."
  (let ((image (im:create width height :color-space-rgb :data-type-byte))
        (state 12345))
    (flet ((next ()
             (setf state (ldb (byte 32 0) (+ (* state 1103515245) 12345)))
             (/ (ldb (byte 15 16) state) 32768.0d0)))
      (dotimes (i (* width height) image)
        ;; One shared component carrying most of the variance, plus a small
        ;; independent one per plane. Without that per-plane jitter the three
        ;; planes differ only by a constant, the covariance is exactly rank 1,
        ;; and every fit reports rank 1 -- which is a degenerate image rather
        ;; than a correlated one.
        (let ((base (+ 128.0d0 (* 40.0d0 (- (next) 0.5d0)))))
          (dotimes (plane 3)
            (setf (cffi:mem-aref (im:plane-pointer image plane) :unsigned-char i)
                  (max 0 (min 255 (round (+ base
                                            (* 6.0d0 (- (next) 0.5d0))
                                            (* -6.0d0 plane))))))))))))

(defun pixel (image plane x y)
  (cffi:mem-aref (im:plane-pointer image plane) :unsigned-char
                 (+ (* y (im:width image)) x)))

(defun set-pixels (image plane value)
  (dotimes (i (im:pixel-count image))
    (setf (cffi:mem-aref (im:plane-pointer image plane) :unsigned-char i) value)))

(defun binary-dumbbell (&key (width 64) (height 40) (radius 11) (overlap 4))
  "A binary image of two overlapping discs -- one connected region, two objects.

The shape that separates IM:FIND-REGIONS from IM:WATERSHED-SEGMENT. Connected
component labelling has to call this one region, because it is one; only a
watershed of the distance transform splits it at the neck. OVERLAP is how far
the two discs are pushed into each other, in pixels."
  (let* ((image (im:create width height :color-space-binary :data-type-byte))
         (plane (im:plane-pointer image 0))
         (cy (floor height 2))
         (offset (- radius (floor overlap 2)))
         (cx1 (- (floor width 2) offset))
         (cx2 (+ (floor width 2) offset)))
    (dotimes (i (* width height))
      (setf (cffi:mem-aref plane :unsigned-char i) 0))
    (loop for y below height
          do (loop for x below width
                   when (or (<= (+ (expt (- x cx1) 2) (expt (- y cy) 2))
                                (* radius radius))
                            (<= (+ (expt (- x cx2) 2) (expt (- y cy) 2))
                                (* radius radius)))
                     do (setf (cffi:mem-aref plane :unsigned-char
                                             (+ (* y width) x))
                              1)))
    image))

(defun noisy-step (&key (width 64) (height 64) (low 60) (high 190) (noise 20))
  "A gray byte image of two flat halves with a hard vertical edge, plus noise.

What an edge-preserving filter is for: a denoising filter that works has to
reduce the variation WITHIN each half without softening the step BETWEEN them,
and those two are separable only on an image that has both. Deterministic."
  (let ((image (im:create width height :color-space-gray :data-type-byte))
        (state 20260912))
    (flet ((next ()
             (setf state (ldb (byte 32 0) (+ (* state 1103515245) 12345)))
             (- (mod (ldb (byte 15 16) state) (1+ (* 2 noise))) noise)))
      (dotimes (i (* width height) image)
        (let ((level (if (< (mod i width) (floor width 2)) low high)))
          (setf (cffi:mem-aref (im:plane-pointer image 0) :unsigned-char i)
                (max 0 (min 255 (+ level (next))))))))))
