(defpackage #:im-convolve
  (:use #:common-lisp)
  (:export #:convolve
           #:convolve-sep
           #:convolve-dual
           #:convolve-rep
           #:compass-convolve
           #:rotate-kernel
           #:diff-of-gaussian
           #:lap-of-gaussian
           #:mean
           #:gaussian
           #:barlett
           #:median
           #:range
           #:rank-closest
           #:rank-max
           #:rank-min
           #:sobel
           #:prewitt
           #:spline-edge
           #:zero-crossing
           #:canny
           #:unsharp
           #:sharp
           #:sharp-kernel))

(in-package #:im-convolve)

(defun convolve (src-image dst-image kernel)
  "2D convolution with KERNEL (an imImage)."
  (im-process-cffi::%im-process-convolve src-image dst-image kernel))

(defun convolve-sep (src-image dst-image kernel)
  "Separable 2D convolution. KERNEL's first row and first column are
applied independently."
  (im-process-cffi::%im-process-convolve-sep src-image dst-image kernel))

(defun convolve-dual (src-image dst-image kernel1 kernel2)
  "Apply two kernels and combine results (e.g. gradient magnitude
from x and y partials)."
  (im-process-cffi::%im-process-convolve-dual src-image dst-image kernel1 kernel2))

(defun convolve-rep (src-image dst-image kernel count)
  "Repeat KERNEL convolution COUNT times."
  (im-process-cffi::%im-process-convolve-rep src-image dst-image kernel count))

(defun compass-convolve (src-image dst-image kernel)
  "Compass-style convolution: apply KERNEL in 8 rotations and keep
the per-pixel maximum response."
  (im-process-cffi::%im-process-compass-convolve src-image dst-image kernel))

(defun rotate-kernel (kernel)
  "Rotate KERNEL 45 degrees in place (used by COMPASS-CONVOLVE)."
  (im-process-cffi::%im-process-rotate-kernel kernel))

;;; Specific filters ---------------------------------------------------

(defun diff-of-gaussian (src-image dst-image stddev1 stddev2)
  "Difference of two Gaussians."
  (im-process-cffi::%im-process-diff-of-gaussian-convolve
   src-image dst-image (coerce stddev1 'double-float) (coerce stddev2 'double-float)))

(defun lap-of-gaussian (src-image dst-image stddev)
  "Laplacian of Gaussian."
  (im-process-cffi::%im-process-lap-of-gaussian-convolve
   src-image dst-image (coerce stddev 'double-float)))

(defun mean (src-image dst-image kernel-size)
  "Mean (box) filter of side KERNEL-SIZE."
  (im-process-cffi::%im-process-mean-convolve src-image dst-image kernel-size))

(defun gaussian (src-image dst-image stddev)
  "Gaussian blur with given STDDEV."
  (im-process-cffi::%im-process-gaussian-convolve
   src-image dst-image (coerce stddev 'double-float)))

(defun barlett (src-image dst-image kernel-size)
  "Barlett (triangular) filter."
  (im-process-cffi::%im-process-barlett-convolve src-image dst-image kernel-size))

(defun median (src-image dst-image kernel-size)
  "Median filter — useful for salt-and-pepper noise."
  (im-process-cffi::%im-process-median-convolve src-image dst-image kernel-size))

(defun range (src-image dst-image kernel-size)
  "Range filter: per-pixel local (max - min)."
  (im-process-cffi::%im-process-range-convolve src-image dst-image kernel-size))

(defun rank-closest (src-image dst-image kernel-size)
  "Replace each pixel with the kernel value closest to the pixel itself."
  (im-process-cffi::%im-process-rank-closest-convolve src-image dst-image kernel-size))

(defun rank-max (src-image dst-image kernel-size)
  "Local max (max-rank filter)."
  (im-process-cffi::%im-process-rank-max-convolve src-image dst-image kernel-size))

(defun rank-min (src-image dst-image kernel-size)
  "Local min (min-rank filter)."
  (im-process-cffi::%im-process-rank-min-convolve src-image dst-image kernel-size))

;;; Edge detectors ----------------------------------------------------

(defun sobel (src-image dst-image)
  "Sobel edge magnitude."
  (im-process-cffi::%im-process-sobel-convolve src-image dst-image))

(defun prewitt (src-image dst-image)
  "Prewitt edge magnitude."
  (im-process-cffi::%im-process-prewitt-convolve src-image dst-image))

(defun spline-edge (src-image dst-image)
  "Spline-based edge detector."
  (im-process-cffi::%im-process-spline-edge-convolve src-image dst-image))

(defun zero-crossing (src-image dst-image)
  "Zero-crossing detector (typically applied to LapOfGaussian output)."
  (im-process-cffi::%im-process-zero-crossing src-image dst-image))

(defun canny (src-image dst-image &optional (stddev 1.0))
  "Canny edge detector with given Gaussian STDDEV."
  (im-process-cffi::%im-process-canny src-image dst-image (coerce stddev 'double-float)))

;;; Sharpening --------------------------------------------------------

(defun unsharp (src-image dst-image stddev amount threshold)
  "Unsharp mask: dst = src + amount * (src - gaussian(src, stddev))."
  (im-process-cffi::%im-process-unsharp src-image dst-image
                                        (coerce stddev 'double-float)
                                        (coerce amount 'double-float)
                                        (coerce threshold 'double-float)))

(defun sharp (src-image dst-image amount threshold)
  "High-pass sharpening."
  (im-process-cffi::%im-process-sharp src-image dst-image
                                      (coerce amount 'double-float)
                                      (coerce threshold 'double-float)))

(defun sharp-kernel (src-image kernel dst-image amount threshold)
  "Sharpen using a custom KERNEL."
  (im-process-cffi::%im-process-sharp-kernel src-image kernel dst-image
                                             (coerce amount 'double-float)
                                             (coerce threshold 'double-float)))
