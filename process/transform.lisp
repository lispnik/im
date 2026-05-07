(defpackage #:im-transform
  (:use #:common-lisp)
  (:export #:fft
           #:ifft
           #:fft-raw
           #:swap-quadrants
           #:cross-correlation
           #:auto-correlation
           #:distance-transform
           #:regional-maximum
           #:hough-lines
           #:hough-lines-draw
           #:openmp-set-min-count
           #:openmp-set-num-threads))

(in-package #:im-transform)

(defun fft (src-image dst-image)
  "Forward 2D FFT. SRC-IMAGE may be real or complex; DST-IMAGE must
be complex (CFLOAT or CDOUBLE)."
  (im-process-cffi::%im-process-fft src-image dst-image))

(defun ifft (src-image dst-image)
  "Inverse 2D FFT."
  (im-process-cffi::%im-process-ifft src-image dst-image))

(defun fft-raw (image &key inverse center normalize)
  "In-place FFT. INVERSE selects forward (NIL) vs inverse (T).
CENTER selects DC at corner (NIL) vs centred (T). NORMALIZE divides
by N for the forward transform."
  (im-process-cffi::%im-process-fft-raw
   image (if inverse 1 0) (if center 1 0) (if normalize 1 0)))

(defun swap-quadrants (image &key center-to-origin)
  "Swap quadrants of an FFT result for visualisation. CENTER-TO-ORIGIN
selects which direction the swap goes."
  (im-process-cffi::%im-process-swap-quadrants image (if center-to-origin 1 0)))

(defun cross-correlation (src-image1 src-image2 dst-image)
  "Compute 2D cross-correlation via FFT."
  (im-process-cffi::%im-process-cross-correlation src-image1 src-image2 dst-image))

(defun auto-correlation (src-image dst-image)
  "Compute 2D auto-correlation."
  (im-process-cffi::%im-process-auto-correlation src-image dst-image))

(defun distance-transform (src-image dst-image)
  "Distance-from-background at each foreground pixel; background
pixels are left untouched (callers should zero-init DST-IMAGE first)."
  (im-process-cffi::%im-process-distance-transform src-image dst-image))

(defun regional-maximum (src-image dst-image)
  "Mark pixels that are local maxima of their connected neighbourhood.
SRC-IMAGE must be IM_FLOAT or IM_DOUBLE."
  (im-process-cffi::%im-process-regional-maximum src-image dst-image))

(defun hough-lines (src-image dst-image)
  "Hough transform for line detection."
  (im-process-cffi::%im-process-hough-lines src-image dst-image))

(defun hough-lines-draw (src-image hough hough-points dst-image)
  "Draw the lines detected via HOUGH-LINES on top of SRC-IMAGE."
  (im-process-cffi::%im-process-hough-lines-draw
   src-image hough hough-points dst-image))

;;; OpenMP control ----------------------------------------------------

(defun openmp-set-min-count (min-count)
  "Set the minimum pixel count below which OpenMP parallelism is
disabled. Returns the previous setting."
  (im-process-cffi::%im-process-openmp-set-min-count min-count))

(defun openmp-set-num-threads (count)
  "Set the maximum thread count for OpenMP-built libim_process_omp.
Returns the previous setting."
  (im-process-cffi::%im-process-openmp-set-num-threads count))
