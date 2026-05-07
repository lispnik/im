(defpackage #:im-color
  (:use #:common-lisp)
  (:export #:replace-color
           #:set-alpha-color
           #:pseudo-color
           #:fix-bgr
           #:select-hue
           #:select-hsi
           #:shift-hsi
           #:shift-component
           #:split-y-chroma
           #:split-hsi
           #:merge-hsi
           #:split-components
           #:merge-components
           #:normalize-components
           #:quantize-rgb-uniform
           #:quantize-rgb-median-cut
           #:quantize-gray-uniform
           #:quantize-gray-median-cut
           #:expand-histogram
           #:equalize-histogram))

(in-package #:im-color)

(defun replace-color (src-image dst-image src-color dst-color)
  "Replace pixels matching SRC-COLOR (per-plane sequence) with
DST-COLOR (per-plane sequence)."
  (let ((n (length src-color)))
    (cffi:with-foreign-objects ((sp :double n)
                                (dp :double n))
      (loop for i below n
            for s across (coerce src-color 'vector)
            for d across (coerce dst-color 'vector)
            do (setf (cffi:mem-aref sp :double i) (coerce s 'double-float)
                     (cffi:mem-aref dp :double i) (coerce d 'double-float)))
      (im-process-cffi::%im-process-replace-color src-image dst-image sp dp))))

(defun set-alpha-color (src-image dst-image src-color dst-alpha)
  "Set the alpha channel where pixels match SRC-COLOR."
  (let ((n (length src-color)))
    (cffi:with-foreign-object (sp :double n)
      (loop for i below n
            for s across (coerce src-color 'vector)
            do (setf (cffi:mem-aref sp :double i) (coerce s 'double-float)))
      (im-process-cffi::%im-process-set-alpha-color
       src-image dst-image sp (coerce dst-alpha 'double-float)))))

(defun pseudo-color (src-image dst-image)
  "Map gray values to a continuous hue (HSI -> RGB).  Note: this
ignores the source image's palette."
  (im-process-cffi::%im-process-pseudo-color src-image dst-image))

(defun fix-bgr (src-image dst-image)
  "Swap red and blue planes (BGR <-> RGB)."
  (im-process-cffi::%im-process-fix-bgr src-image dst-image))

(defun select-hue (src-image dst-image hue-start hue-end)
  "Keep pixels whose hue is in [HUE-START, HUE-END]; clear others."
  (im-process-cffi::%im-process-select-hue
   src-image dst-image
   (coerce hue-start 'double-float)
   (coerce hue-end 'double-float)))

(defun select-hsi (src-image dst-image hue-start hue-end sat-start sat-end int-start int-end)
  "Keep pixels whose HSI components fall in the given ranges."
  (im-process-cffi::%im-process-select-hsi
   src-image dst-image
   (coerce hue-start 'double-float)
   (coerce hue-end 'double-float)
   (coerce sat-start 'double-float)
   (coerce sat-end 'double-float)
   (coerce int-start 'double-float)
   (coerce int-end 'double-float)))

(defun shift-hsi (src-image dst-image h-shift s-shift i-shift)
  "Add per-component shifts to the HSI representation of SRC-IMAGE."
  (im-process-cffi::%im-process-shift-hsi
   src-image dst-image
   (coerce h-shift 'double-float)
   (coerce s-shift 'double-float)
   (coerce i-shift 'double-float)))

(defun shift-component (src-image dst-image c0-shift c1-shift c2-shift)
  "Add per-plane shifts to SRC-IMAGE."
  (im-process-cffi::%im-process-shift-component
   src-image dst-image
   (coerce c0-shift 'double-float)
   (coerce c1-shift 'double-float)
   (coerce c2-shift 'double-float)))

;;; Split / merge -----------------------------------------------------

(defun split-y-chroma (src-image y-image chroma-image)
  "Split YCbCr SRC into a luma image (Y) and a chroma image (Cb/Cr packed)."
  (im-process-cffi::%im-process-split-y-chroma src-image y-image chroma-image))

(defun split-hsi (src-image h-image s-image i-image)
  "Split RGB SRC into separate Hue, Saturation, Intensity float planes."
  (im-process-cffi::%im-process-split-hsi src-image h-image s-image i-image))

(defun merge-hsi (h-image s-image i-image dst-image)
  "Inverse of SPLIT-HSI."
  (im-process-cffi::%im-process-merge-hsi h-image s-image i-image dst-image))

(defun split-components (src-image &rest dst-images)
  "Split each plane of SRC-IMAGE into a corresponding gray DST-IMAGE."
  (let ((n (length dst-images)))
    (cffi:with-foreign-object (lp :pointer n)
      (loop for i below n
            for img in dst-images
            do (setf (cffi:mem-aref lp :pointer i) img))
      (im-process-cffi::%im-process-split-components src-image lp))))

(defun merge-components (image-list dst-image)
  "Inverse of SPLIT-COMPONENTS: merge a list of gray images into a
multi-plane image."
  (let ((n (length image-list)))
    (cffi:with-foreign-object (lp :pointer n)
      (loop for i below n
            for img in image-list
            do (setf (cffi:mem-aref lp :pointer i) img))
      (im-process-cffi::%im-process-merge-components lp dst-image))))

(defun normalize-components (src-image dst-image)
  "Normalize each colour plane to span [0, 1] independently."
  (im-process-cffi::%im-process-normalize-components src-image dst-image))

;;; Quantize ----------------------------------------------------------

(defun quantize-rgb-uniform (src-image dst-image &key dither)
  "Quantize RGB SRC-IMAGE to a uniform 6x6x6 (216) palette MAP image.
With DITHER T, applies error diffusion."
  (im-process-cffi::%im-process-quantize-rgb-uniform
   src-image dst-image (if dither t nil)))

(defun quantize-rgb-median-cut (src-image dst-image)
  "Quantize RGB SRC-IMAGE to a MAP image using median-cut palette
selection."
  (im-process-cffi::%im-process-quantize-rgb-median-cut src-image dst-image))

(defun quantize-gray-uniform (src-image dst-image grays)
  "Quantize a gray image to GRAYS distinct levels."
  (im-process-cffi::%im-process-quantize-gray-uniform
   src-image dst-image grays))

(defun quantize-gray-median-cut (src-image dst-image grays)
  "Quantize a gray image using median-cut to GRAYS levels."
  (im-process-cffi::%im-process-quantize-gray-median-cut
   src-image dst-image grays))

;;; Histogram ---------------------------------------------------------

(defun expand-histogram (src-image dst-image &key (percent 0.0))
  "Linearly stretch the image histogram, ignoring PERCENT of pixels
at each tail."
  (im-process-cffi::%im-process-expand-histogram
   src-image dst-image (coerce percent 'double-float)))

(defun equalize-histogram (src-image dst-image)
  "Histogram equalization."
  (im-process-cffi::%im-process-equalize-histogram src-image dst-image))
