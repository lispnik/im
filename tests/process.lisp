;;;; tests/process.lisp — the processing operations.

(in-package #:im.tests)

(def-suite process-suite :in im-suite
  :description "Geometry, filtering, thresholding, morphology and analysis.")
(in-suite process-suite)

(test resize-changes-dimensions-and-keeps-content
  (im:with-images ((source (gray-gradient 64 64))
                   (dest (im:create 32 32 :color-space-gray :data-type-byte)))
    (finishes (im:resize source dest))
    (is (= 32 (im:width dest)))
    ;; A resampled gradient is still a gradient: the last sample must exceed
    ;; the first, which a blank destination would not satisfy.
    (is (> (pixel dest 0 31 31) (pixel dest 0 0 0)))))

(test rotate-90-exchanges-width-and-height
  (im:with-images ((source (gray-gradient 40 20))
                   (dest (im:create 20 40 :color-space-gray :data-type-byte)))
    (finishes (im:rotate-90 source dest 1))
    (is (= 20 (im:width dest)))
    (is (= 40 (im:height dest)))))

(test crop-extracts-a-region
  (im:with-images ((source (gray-gradient 32 32))
                   (dest (im:create 8 8 :color-space-gray :data-type-byte)))
    (finishes (im:crop source dest 4 4))
    (is (= 8 (im:width dest)))
    (is (= (pixel source 0 4 4) (pixel dest 0 0 0))
        "the cropped origin must be the source pixel at the crop offset")))

(test negative-inverts-samples
  (im:with-images ((source (im:create 8 8 :color-space-gray :data-type-byte))
                   (dest (im:create 8 8 :color-space-gray :data-type-byte)))
    (set-pixels source 0 200)
    (finishes (im:negative source dest))
    (is (= 55 (pixel dest 0 0 0)) "255 - 200")))

(test sobel-finds-an-edge-and-not-a-flat-field
  (im:with-images ((flat (im:create 32 32 :color-space-gray :data-type-byte))
                   (flat-edges (im:create 32 32 :color-space-gray :data-type-byte))
                   (blocky (binary-block :width 32 :height 32))
                   (gray (im:create 32 32 :color-space-gray :data-type-byte))
                   (edges (im:create 32 32 :color-space-gray :data-type-byte)))
    (set-pixels flat 0 128)
    (im:convolve-sobel flat flat-edges)
    (is (zerop (pixel flat-edges 0 16 16)) "a flat field has no edges")
    ;; Promote the binary block to gray so Sobel has something to work on.
    (dotimes (i (im:pixel-count blocky))
      (setf (cffi:mem-aref (im:plane-pointer gray 0) :unsigned-char i)
            (* 255 (cffi:mem-aref (im:plane-pointer blocky 0) :unsigned-char i))))
    (im:convolve-sobel gray edges)
    (is (plusp (loop for i below (im:pixel-count edges)
                     sum (cffi:mem-aref (im:plane-pointer edges 0) :unsigned-char i)))
        "a block has edges")))

(test gaussian-blur-reduces-variation
  (im:with-images ((source (gray-gradient 64 64))
                   (dest (im:create 64 64 :color-space-gray :data-type-byte)))
    (im:convolve-gaussian source dest 3.0d0)
    (is (< (getf (im:statistics dest) :stddev)
           (getf (im:statistics source) :stddev))
        "blurring must reduce the standard deviation")))

(test otsu-returns-the-level-it-used
  (im:with-images ((source (gray-gradient 64 64))
                   (dest (im:create 64 64 :color-space-binary :data-type-byte)))
    (let ((level (im:threshold-otsu source dest)))
      (is (integerp level))
      (is (<= 0 level 255))
      (is (eq :color-space-binary (im:color-space dest))))))

(test morphology-dispatches-on-colour-space
  "Binary images must use the binary operators, greyscale the greyscale ones."
  (im:with-images ((source (binary-block :width 32 :height 32 :x 8 :y 8 :size 8))
                   (eroded (im:create 32 32 :color-space-binary :data-type-byte))
                   (dilated (im:create 32 32 :color-space-binary :data-type-byte)))
    (flet ((set-pixels-count (image)
             (loop for i below (im:pixel-count image)
                   count (plusp (cffi:mem-aref (im:plane-pointer image 0)
                                               :unsigned-char i)))))
      (im:morph-erode source eroded)
      (im:morph-dilate source dilated)
      (is (< (set-pixels-count eroded) 64) "erosion must shrink the block")
      (is (> (set-pixels-count dilated) 64) "dilation must grow it"))))

(test statistics-describe-the-image
  (im:with-image (image (im:create 16 16 :color-space-gray :data-type-byte))
    (set-pixels image 0 100)
    (let ((stats (im:statistics image)))
      (is (= 100 (getf stats :max)))
      (is (= 100 (getf stats :min)))
      (is (= 100 (getf stats :mean)))
      (is (zerop (getf stats :stddev)))
      (is (= 256 (getf stats :positive))))))

(test statistics-plane-is-bounds-checked
  (im:with-image (image (im:create 8 8 :color-space-gray :data-type-byte))
    (signals im:im-error (im:statistics image 5))))

(test rms-error-is-zero-for-identical-images
  (im:with-images ((a (gray-gradient 32 32))
                   (b (gray-gradient 32 32)))
    (is (zerop (im:rms-error a b)))))

(test find-regions-counts-connected-components
  (im:with-image (source (binary-block :width 64 :height 64 :x 4 :y 4 :size 6))
    (multiple-value-bind (labelled count) (im:find-regions source)
      (im:with-image (labelled labelled)
        (is (= 1 count) "one block means one region")
        (let ((areas (im:region-areas labelled count)))
          (is (= 1 (length areas)))
          (is (= 36 (aref areas 0)) "a 6x6 block has 36 pixels"))
        (let ((centroids (im:region-centroids labelled count)))
          (is (= 1 (length centroids)))
          ;; A 6x6 block at (4,4) is centred near (6.5, 6.5). Reading the
          ;; doubles as floats gave 4.07e9 here.
          (is (< 5.0d0 (car (aref centroids 0)) 8.0d0))
          (is (< 5.0d0 (cdr (aref centroids 0)) 8.0d0)))))))

(test find-regions-rejects-a-wrongly-typed-destination
  "IM documents a gray ushort label image and does not check it."
  (im:with-images ((source (binary-block))
                   (wrong (im:create 32 32 :color-space-gray :data-type-int)))
    (signals im:data-error (im:find-regions source wrong))))

(test fourier-round-trip-preserves-the-image
  "FFT then IFFT returns what went in, whichever backend is in use.

Note the second complex image: IM requires BOTH arguments of the inverse
transform to be complex, and getting back to a real image is a separate
conversion step."
  (im:with-images ((source (gray-gradient 32 32))
                   (spectrum (im:create 32 32 :color-space-gray :data-type-cfloat))
                   (inverse (im:create 32 32 :color-space-gray :data-type-cfloat))
                   (restored (im:create 32 32 :color-space-gray :data-type-byte)))
    (im:fft source spectrum)
    (im:ifft spectrum inverse)
    (im:convert-data-type inverse restored
                          :complex-part :real
                          :cast-mode :direct)
    (is (< (im:rms-error source restored) 2.0d0)
        "round-tripped image differs from the original by too much")))

(test fft-preconditions-signal-instead-of-crashing
  "A real destination is a documented precondition violation, not a segfault."
  (im:with-images ((source (gray-gradient 16 16))
                   (real-dst (im:create 16 16 :color-space-gray :data-type-byte))
                   (complex-dst (im:create 16 16 :color-space-gray :data-type-cfloat))
                   (wrong-size (im:create 8 8 :color-space-gray :data-type-cfloat)))
    (signals im:data-error (im:fft source real-dst))
    (signals im:data-error (im:fft source wrong-size))
    ;; The inverse additionally requires a complex SOURCE.
    (signals im:data-error (im:ifft source complex-dst))))
