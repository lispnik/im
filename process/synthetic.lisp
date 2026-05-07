(defpackage #:im-render
  (:use #:common-lisp)
  (:export #:random-noise
           #:constant
           #:wheel
           #:cone
           #:tent
           #:ramp
           #:box
           #:sinc
           #:gaussian
           #:lap-of-gaussian
           #:cosine
           #:grid
           #:chessboard
           #:flood-fill
           #:add-speckle-noise
           #:add-gaussian-noise
           #:add-uniform-noise))

(in-package #:im-render)

(defun random-noise (im-image)
  "Fill IM-IMAGE with random noise. Signals COUNTER-ABORTED."
  (im-process-cffi::%im-process-render-random-noise im-image))

(defun constant (im-image &rest plane-values)
  "Fill IM-IMAGE plane-by-plane with PLANE-VALUES. The number of
values should match the image depth (alpha included). Signals
COUNTER-ABORTED."
  (let ((n (length plane-values)))
    (cffi:with-foreign-object (vp :double n)
      (loop for i below n
            for v in plane-values
            do (setf (cffi:mem-aref vp :double i) (coerce v 'double-float)))
      (im-process-cffi::%im-process-render-constant im-image vp))))

(defun wheel (im-image internal-radius external-radius)
  "Draw a colour wheel. Signals COUNTER-ABORTED."
  (im-process-cffi::%im-process-render-wheel im-image internal-radius external-radius))

(defun cone (im-image radius)
  "Draw a cone. Signals COUNTER-ABORTED."
  (im-process-cffi::%im-process-render-cone im-image radius))

(defun tent (im-image tent-width tent-height)
  "Render a tent function. Signals COUNTER-ABORTED."
  (im-process-cffi::%im-process-render-tent im-image tent-width tent-height))

(defun ramp (im-image start end &key vertical)
  "Linear ramp from START to END across the image. VERTICAL selects
direction. Signals COUNTER-ABORTED."
  (im-process-cffi::%im-process-render-ramp im-image start end (if vertical t nil)))

(defun box (im-image width height)
  "Render a centred bright box. Signals COUNTER-ABORTED."
  (im-process-cffi::%im-process-render-box im-image width height))

(defun sinc (im-image x-period y-period)
  "Render a 2D sinc pattern. Signals COUNTER-ABORTED."
  (im-process-cffi::%im-process-render-sinc im-image
                                            (coerce x-period 'double-float)
                                            (coerce y-period 'double-float)))

(defun gaussian (im-image stddev)
  "Render a 2D Gaussian centred at the image centre. Signals
COUNTER-ABORTED."
  (im-process-cffi::%im-process-render-gaussian im-image (coerce stddev 'double-float)))

(defun lap-of-gaussian (im-image stddev)
  "Render a Laplacian-of-Gaussian. Signals COUNTER-ABORTED."
  (im-process-cffi::%im-process-render-lap-of-gaussian
   im-image (coerce stddev 'double-float)))

(defun cosine (im-image x-period y-period)
  "Render a 2D cosine. Signals COUNTER-ABORTED."
  (im-process-cffi::%im-process-render-cosine im-image
                                              (coerce x-period 'double-float)
                                              (coerce y-period 'double-float)))

(defun grid (im-image x-space y-space)
  "Render a regular grid of bright lines spaced X-SPACE / Y-SPACE
pixels apart. Signals COUNTER-ABORTED."
  (im-process-cffi::%im-process-render-grid im-image
                                            (coerce x-space 'double-float)
                                            (coerce y-space 'double-float)))

(defun chessboard (im-image x-space y-space)
  "Render a chessboard pattern with squares of size X-SPACE x Y-SPACE.
Note: the pattern is centred at (W/2, H/2); odd image sizes can
produce a discontinuity at the centre line. Signals COUNTER-ABORTED."
  (im-process-cffi::%im-process-render-chessboard im-image
                                                  (coerce x-space 'double-float)
                                                  (coerce y-space 'double-float)))

(defun flood-fill (im-image start-x start-y replace-color tolerance)
  "Flood-fill from (START-X, START-Y) replacing connected pixels
within TOLERANCE of the starting colour with REPLACE-COLOR.
REPLACE-COLOR is a sequence of plane values (length = image depth)."
  (let ((n (length replace-color)))
    (cffi:with-foreign-object (vp :double n)
      (loop for i below n
            for v across (coerce replace-color 'vector)
            do (setf (cffi:mem-aref vp :double i) (coerce v 'double-float)))
      (im-process-cffi::%im-process-render-flood-fill
       im-image start-x start-y vp (coerce tolerance 'double-float)))))

(defun add-speckle-noise (src-image dst-image percent)
  "Apply speckle noise to SRC-IMAGE. PERCENT is the percentage of
pixels affected. Signals COUNTER-ABORTED."
  (im-process-cffi::%im-process-render-add-speckle-noise
   src-image dst-image (coerce percent 'double-float)))

(defun add-gaussian-noise (src-image dst-image mean stddev)
  "Add Gaussian noise with given MEAN and STDDEV. Signals COUNTER-ABORTED."
  (im-process-cffi::%im-process-render-add-gaussian-noise
   src-image dst-image (coerce mean 'double-float) (coerce stddev 'double-float)))

(defun add-uniform-noise (src-image dst-image mean stddev)
  "Add uniformly distributed noise with given MEAN and STDDEV.
Signals COUNTER-ABORTED."
  (im-process-cffi::%im-process-render-add-uniform-noise
   src-image dst-image (coerce mean 'double-float) (coerce stddev 'double-float)))
