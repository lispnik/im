(defpackage #:im-threshold
  (:use #:common-lisp)
  (:export #:threshold
           #:threshold-by-diff
           #:threshold-color
           #:threshold-saturation
           #:hysteresis
           #:hysteresis-estimate
           #:uniform-err
           #:diffusion-err
           #:percent
           #:otsu
           #:min-max
           #:local-max
           #:local-max-estimate
           #:range-contrast
           #:slice))

(in-package #:im-threshold)

(defun threshold (src-image dst-image level &key (value 1))
  "Threshold to binary: dst = VALUE where src > LEVEL, 0 elsewhere."
  (im-process-cffi::%im-process-threshold src-image dst-image
                                          (coerce level 'double-float)
                                          value))

(defun threshold-by-diff (src-image1 src-image2 dst-image)
  "Per-pixel comparison: dst = 1 where src1 > src2 (NOT a magnitude
comparison despite the name)."
  (im-process-cffi::%im-process-threshold-by-diff src-image1 src-image2 dst-image))

(defun threshold-color (src-image dst-image src-color tolerance)
  "Mark pixels whose RGB triple is within TOLERANCE of SRC-COLOR.
SRC-COLOR is a list/vector of length matching SRC-IMAGE depth."
  (let ((n (length src-color)))
    (cffi:with-foreign-object (cp :double n)
      (loop for i below n
            for v across (coerce src-color 'vector)
            do (setf (cffi:mem-aref cp :double i) (coerce v 'double-float)))
      (im-process-cffi::%im-process-threshold-color
       src-image dst-image cp (coerce tolerance 'double-float)))))

(defun threshold-saturation (src-image dst-image s-min)
  "Threshold by HSI saturation: keep pixels with S >= S-MIN."
  (im-process-cffi::%im-process-threshold-saturation
   src-image dst-image (coerce s-min 'double-float)))

(defun hysteresis (src-image dst-image low-thres high-thres)
  "Hysteresis threshold: pixels >= HIGH-THRES are foreground; pixels
in [LOW-THRES, HIGH-THRES) are foreground only if connected to a
foreground pixel."
  (im-process-cffi::%im-process-hysteresis-threshold
   src-image dst-image low-thres high-thres))

(defun hysteresis-estimate (image)
  "Suggested (LOW HIGH) threshold levels for IMAGE; returns two values."
  (cffi:with-foreign-objects ((lo :int) (hi :int))
    (im-process-cffi::%im-process-hysteresis-thres-estimate image lo hi)
    (values (cffi:mem-ref lo :int) (cffi:mem-ref hi :int))))

(defun uniform-err (src-image dst-image)
  "Uniform-error thresholding."
  (im-process-cffi::%im-process-uniform-err-threshold src-image dst-image))

(defun diffusion-err (src-image dst-image level)
  "Error-diffusion thresholding (Floyd-Steinberg style)."
  (im-process-cffi::%im-process-diffusion-err-threshold src-image dst-image level))

(defun percent (src-image dst-image percent)
  "Threshold so that PERCENT of pixels are foreground."
  (im-process-cffi::%im-process-percent-threshold
   src-image dst-image (coerce percent 'double-float)))

(defun otsu (src-image dst-image)
  "Otsu's method (between-class variance maximisation). Returns the
chosen threshold level."
  (im-process-cffi::%im-process-otsu-threshold src-image dst-image))

(defun min-max (src-image dst-image)
  "Threshold at the midpoint of the image's min/max range. Returns
the chosen threshold."
  (im-process-cffi::%im-process-min-max-threshold src-image dst-image))

(defun local-max (src-image dst-image kernel-size min-level)
  "Threshold pixels that are local maxima within KERNEL-SIZE windows
and exceed MIN-LEVEL."
  (im-process-cffi::%im-process-local-max-threshold
   src-image dst-image kernel-size min-level))

(defun local-max-estimate (image)
  "Suggest a level for LOCAL-MAX threshold."
  (cffi:with-foreign-object (lvl :int)
    (im-process-cffi::%im-process-local-max-thres-estimate image lvl)
    (cffi:mem-ref lvl :int)))

(defun range-contrast (src-image dst-image kernel-size min-range)
  "Threshold pixels whose local (max - min) exceeds MIN-RANGE."
  (im-process-cffi::%im-process-range-contrast-threshold
   src-image dst-image kernel-size min-range))

(defun slice (src-image dst-image start-level end-level)
  "Mark pixels with values inside [START-LEVEL, END-LEVEL]."
  (im-process-cffi::%im-process-slice-threshold
   src-image dst-image
   (coerce start-level 'double-float)
   (coerce end-level 'double-float)))
