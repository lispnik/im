(in-package #:im-tests)

;;; Synthetic-image helpers shared across the test suite. These mirror
;;; the C-side helpers in tecgraf/test/im/test_helpers.h so individual
;;; assertions can lean on the same patterns the C tests use.

(defparameter *tmp-dir*
  (uiop:ensure-directory-pathname
   (uiop:merge-pathnames* "imtests-cl/" (uiop:temporary-directory)))
  "Per-session scratch directory; cleared at the start of the suite.")

(defun ensure-tmp-dir ()
  (ensure-directories-exist *tmp-dir*))

(defun tmp-path (name)
  "Return an absolute pathname under *TMP-DIR*."
  (ensure-tmp-dir)
  (uiop:merge-pathnames* name *tmp-dir*))

(defmacro with-image ((var ctor-form) &body body)
  "Bind VAR to the image returned by CTOR-FORM and ensure it is
destroyed on exit."
  `(let ((,var ,ctor-form))
     (unwind-protect (progn ,@body)
       (when ,var (im-image:destroy ,var)))))

(defmacro with-images (bindings &body body)
  "Like WITH-IMAGE but for multiple images."
  (if (null bindings)
      `(progn ,@body)
      `(with-image ,(first bindings)
         (with-images ,(rest bindings) ,@body))))

(defun fill-gray-gradient (im)
  "Fill a gray-byte IM with ((i mod 256))."
  (let ((p (im-image:data im 0))
        (n (* (im-image:width im) (im-image:height im))))
    (loop for i below n
          do (setf (cffi:mem-aref p :unsigned-char i) (logand i #xff)))))

(defun make-gray-gradient (w h)
  "Allocate a gray-byte image and fill with a deterministic gradient."
  (let ((img (im-image:create w h :color-space-gray :data-type-byte)))
    (fill-gray-gradient img)
    img))

(defun fill-rgb-gradient (im)
  "Fill an RGB-byte IM with smooth per-plane gradients."
  (let* ((w (im-image:width im))
         (h (im-image:height im))
         (n (* w h))
         (r (im-image:data im 0))
         (g (im-image:data im 1))
         (b (im-image:data im 2)))
    (loop for y below h do
      (loop for x below w do
        (let ((i (+ (* y w) x)))
          (setf (cffi:mem-aref r :unsigned-char i)
                (floor (* x 255) (max 1 (1- w))))
          (setf (cffi:mem-aref g :unsigned-char i)
                (floor (* y 255) (max 1 (1- h))))
          (setf (cffi:mem-aref b :unsigned-char i)
                (floor (* (+ x y) 255) (max 1 (+ w h -2)))))))
    im))

(defun make-rgb-gradient (w h)
  (fill-rgb-gradient
   (im-image:create w h :color-space-rgb :data-type-byte)))

(defun make-binary-block (&key (width 16) (height 16) (x0 6) (y0 6) (sz 4))
  "A binary image with an SZ x SZ block of foreground starting at (X0, Y0)."
  (let* ((img (im-image:create width height :color-space-binary :data-type-byte))
         (p (im-image:data img 0)))
    (dotimes (i (* width height))
      (setf (cffi:mem-aref p :unsigned-char i) 0))
    (loop for y from y0 below (+ y0 sz) do
      (loop for x from x0 below (+ x0 sz) do
        (setf (cffi:mem-aref p :unsigned-char (+ (* y width) x)) 1)))
    img))

(defun pixel-byte (im plane x y)
  (cffi:mem-aref (im-image:data im plane)
                 :unsigned-char
                 (+ (* y (im-image:width im)) x)))

(defun (setf pixel-byte) (value im plane x y)
  (setf (cffi:mem-aref (im-image:data im plane)
                       :unsigned-char
                       (+ (* y (im-image:width im)) x))
        value))

(defun image-byte-max-diff (a b)
  "Maximum absolute byte difference across all planes between two
matching-shape byte images. Returns -1 if shapes don't match."
  (cond ((not (im-image:match-p a b)) -1)
        (t (loop with worst = 0
                 for plane below (im-image:depth a)
                 for pa = (im-image:data a plane)
                 for pb = (im-image:data b plane)
                 for n = (* (im-image:width a) (im-image:height a))
                 do (loop for i below n
                          for d = (abs (- (cffi:mem-aref pa :unsigned-char i)
                                          (cffi:mem-aref pb :unsigned-char i)))
                          when (> d worst) do (setf worst d))
                 finally (return worst)))))

(defun count-set-pixels (im)
  "Count nonzero bytes in plane 0 (handy for binary images)."
  (loop with p = (im-image:data im 0)
        with n = (* (im-image:width im) (im-image:height im))
        for i below n
        count (not (zerop (cffi:mem-aref p :unsigned-char i)))))
