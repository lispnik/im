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

;;; Decorrelation stretch -----------------------------------------------------

(test decorrelation-stretch-changes-a-correlated-image
  (im:with-images ((source (correlated-rgb))
                   (dest (im:create 48 32 :color-space-rgb :data-type-byte)))
    (finishes (im:decorrelation-stretch source dest
                                        :space :decorrelation-space-rgb
                                        :scale 3.0d0))
    ;; An operation that copied its input, or wrote a constant, would satisfy
    ;; a weaker assertion than this one.
    (is (> (im:rms-error source dest) 1.0d0))))

(test decorrelation-fit-then-apply-matches-the-one-shot-call
  (im:with-images ((source (correlated-rgb))
                   (one (im:create 48 32 :color-space-rgb :data-type-byte))
                   (two (im:create 48 32 :color-space-rgb :data-type-byte)))
    (im:decorrelation-stretch source one :space :decorrelation-space-yuv :scale 2.0d0)
    (let ((transform (im:decorrelation-fit source
                                           :space :decorrelation-space-yuv
                                           :scale 2.0d0)))
      (is (= 3 (im:decorrelation-rank transform)))
      (is (eq :decorrelation-space-yuv (im:decorrelation-space transform)))
      (im:decorrelation-apply source two transform)
      ;; The struct made a round trip out to Lisp and back into C, so this
      ;; failing would mean a field was marshalled to the wrong offset.
      (is (zerop (im:rms-error one two))))))

(test decorrelation-fit-reports-a-flat-image-as-rank-zero
  (im:with-images ((source (im:create 16 16 :color-space-rgb :data-type-byte)))
    (dotimes (plane 3) (set-pixels source plane (+ 40 (* 30 plane))))
    (let ((transform (im:decorrelation-fit source :space :decorrelation-space-rgb)))
      (is (zerop (im:decorrelation-rank transform))
          "one colour is no colour cloud, so there is nothing to stretch"))))

(test decorrelation-mask-restricts-the-fit
  (im:with-images ((source (correlated-rgb))
                   (mask (im:create 48 32 :color-space-binary :data-type-byte)))
    (set-pixels mask 0 0)
    (dotimes (i (floor (im:pixel-count mask) 2))
      (setf (cffi:mem-aref (im:plane-pointer mask 0) :unsigned-char i) 1))
    (let ((whole (im:decorrelation-fit source :space :decorrelation-space-rgb))
          (part (im:decorrelation-fit source :space :decorrelation-space-rgb :mask mask)))
      (is (not (equalp (im:decorrelation-mean whole) (im:decorrelation-mean part)))
          "the masked fit must be measured from the masked pixels only"))))

(test decorrelation-refuses-an-unnormalized-real-image-in-lab
  ;; L*a*b* is defined over 0-1 and IM's conversion saturates outside it, so
  ;; without this check the caller gets a flat image and no error at all.
  (im:with-images ((source (im:create 16 16 :color-space-rgb :data-type-float)))
    (dotimes (plane 3)
      (dotimes (i (im:pixel-count source))
        (setf (cffi:mem-aref (im:plane-pointer source plane) :float i)
              (+ 100.0 (* 10.0 plane)))))
    (signals im:im-error
      (im:decorrelation-fit source :space :decorrelation-space-lds))
    ;; and the spaces that do not convert through L*a*b* are left alone
    (finishes (im:decorrelation-fit source :space :decorrelation-space-rgb))))

(test decorrelation-rejects-an-unknown-space
  (im:with-images ((source (correlated-rgb)))
    (signals im:im-error
      (im:decorrelation-fit source :space :decorrelation-space-nonesuch))))

;;; Watershed segmentation ----------------------------------------------------

(test watershed-segment-splits-what-find-regions-cannot
  "Two overlapping discs are one connected region and two objects.

The whole reason IM:WATERSHED-SEGMENT exists. Asserting both halves in one
test because either alone would pass on a broken implementation: a watershed
that found one region would look like correct labelling, and one that found
two would look like correct labelling of an image that was never joined."
  (im:with-image (dumbbell (binary-dumbbell))
    (im:with-images ((connected (im:make-label-image dumbbell))
                     (split (im:make-label-image dumbbell)))
      (is (= 1 (nth-value 1 (im:find-regions dumbbell connected)))
          "the two discs overlap, so they are one connected region")
      (is (= 2 (nth-value 1 (im:watershed-segment dumbbell split)))
          "the watershed must split them at the neck"))))

(test watershed-segment-labels-are-usable-by-the-measurements
  "The result is a label image, indistinguishable from IM:FIND-REGIONS's.

That is the promise: every REGION-* function reads it directly, with no
conversion step. A destination of the wrong type would still produce numbers."
  (im:with-image (dumbbell (binary-dumbbell))
    (multiple-value-bind (labelled count) (im:watershed-segment dumbbell)
      (im:with-image (owned labelled)
        (is (eq :data-type-ushort (im:data-type owned)))
        (is (eq :color-space-gray (im:color-space owned)))
        (let ((areas (im:region-areas owned count))
              (boxes (im:region-bounding-boxes owned count)))
          (is (= 2 (length areas)))
          (is (every #'plusp areas))
          ;; One disc left of centre and one right, which is the only thing
          ;; that says the split ran down the neck rather than across it.
          (is (< (getf (aref boxes 0) :xmax) (getf (aref boxes 1) :xmax))))))))

(test watershed-segment-rejects-a-source-it-cannot-use
  (im:with-image (gray (gray-gradient 32 32))
    (signals im:data-error (im:watershed-segment gray))))

(test marker-driven-watershed-assigns-every-pixel-to-a-marker
  "IM:WATERSHED floods from markers given, and labels nothing else.

The relief is a gradient, so the flood order is decided; two markers at
opposite ends must divide the image between them and leave nothing at 0."
  (im:with-images ((relief (gray-gradient 32 32))
                   (markers (im:create 32 32 :color-space-gray :data-type-ushort))
                   (result (im:create 32 32 :color-space-gray :data-type-ushort)))
    (dotimes (i (im:pixel-count markers))
      (setf (cffi:mem-aref (im:plane-pointer markers 0) :unsigned-short i) 0))
    (setf (cffi:mem-aref (im:plane-pointer markers 0) :unsigned-short 0) 1)
    (setf (cffi:mem-aref (im:plane-pointer markers 0) :unsigned-short
                         (1- (im:pixel-count markers)))
          2)
    (finishes (im:watershed relief markers result))
    (is (zerop (loop for i below (im:pixel-count result)
                     count (zerop (cffi:mem-aref (im:plane-pointer result 0)
                                                 :unsigned-short i))))
        "with MARK-LINES off every pixel belongs to some basin")))

(test watershed-rejects-a-marker-image-of-the-wrong-type
  "A byte marker image is refused, not reported as a cancelled operation.

IM checks this and reports it by returning its counter-abort value, which is
the same zero a cancelled operation returns. Without the check here the error
would arrive as IM:OPERATION-ABORTED saying the user stopped the work."
  (im:with-images ((relief (gray-gradient 16 16))
                   (markers (im:create 16 16 :color-space-gray :data-type-byte))
                   (result (im:create 16 16 :color-space-gray :data-type-ushort)))
    (signals im:data-error (im:watershed relief markers result))))

;;; Edge-preserving denoising -------------------------------------------------

(defun half-variation (image half)
  "Mean absolute difference from the mean, over one half of a NOISY-STEP."
  (let* ((width (im:width image))
         (height (im:height image))
         (columns (if (eq half :left)
                      (loop for x below (floor width 2) collect x)
                      (loop for x from (floor width 2) below width collect x)))
         (samples (loop for y below height
                        append (loop for x in columns collect (pixel image 0 x y))))
         (mean (/ (reduce #'+ samples) (length samples))))
    (/ (reduce #'+ (mapcar (lambda (s) (abs (- s mean))) samples))
       (length samples))))

(defun step-height (image)
  "How large the step between the two halves still is, at the middle row."
  (let ((width (im:width image))
        (row (floor (im:height image) 2)))
    (abs (- (pixel image 0 (- (floor width 2) 1) row)
            (pixel image 0 (floor width 2) row)))))

(test edge-preserving-filters-smooth-within-a-region-and-keep-the-step
  "Each of the three reduces the noise without softening the edge.

Both halves of that matter, and a Gaussian is the control: it reduces the
noise too, and gives up most of the step doing it. A denoising test that only
measured the noise would pass on a plain blur."
  (im:with-image (source (noisy-step))
    (let ((noise (half-variation source :left))
          (step (step-height source)))
      (dolist (filter (list (lambda (s d) (im:denoise-bilateral s d 3.0 25.0))
                            (lambda (s d) (im:denoise-anisotropic-diffusion
                                           s d :iterations 12 :kappa 25.0))
                            (lambda (s d) (im:denoise-non-local-means
                                           s d :search-radius 4 :patch-radius 1
                                             :filter-stddev 25.0))))
        (im:with-image (result (im:create-based source))
          (funcall filter source result)
          (is (< (half-variation result :left) noise)
              "the filter must reduce the variation inside a flat region")
          (is (> (step-height result) (* 0.8 step))
              "and must keep at least four fifths of the edge"))))))

(test a-gaussian-loses-the-edge-the-others-keep
  "The control for the test above: this is what not preserving an edge looks like."
  (im:with-image (source (noisy-step))
    (im:with-images ((blurred (im:create-based source))
                     (bilateral (im:create-based source)))
      (im:convolve-gaussian source blurred 3.0)
      (im:denoise-bilateral source bilateral 3.0 25.0)
      (is (> (step-height bilateral) (step-height blurred))
          "a bilateral filter of the same width must keep more of the step"))))

(test anisotropic-diffusion-refuses-an-unstable-time-step
  "Above a quarter the explicit scheme diverges into a checkerboard.

IM checks it, and reports the violation by returning zero -- its cancellation
value. Checked here so it arrives as a bad argument rather than as a
cancelled operation."
  (im:with-image (source (gray-gradient 16 16))
    (im:with-image (result (im:create-based source))
      (signals im:data-error
        (im:denoise-anisotropic-diffusion source result :time-step 0.5d0))
      (signals im:data-error
        (im:denoise-anisotropic-diffusion source result :kappa 0.0d0))
      (signals im:im-error
        (im:denoise-anisotropic-diffusion source result :function :bilateral)))))

(test denoising-rejects-a-destination-of-a-different-type
  (im:with-image (source (gray-gradient 16 16))
    (im:with-image (wrong (im:create-based source :data-type :data-type-float))
      (signals im:data-error (im:denoise-bilateral source wrong 2.0 10.0))
      (signals im:data-error
        (im:denoise-non-local-means source wrong :search-radius 2)))))

;;; Deconvolution -------------------------------------------------------------

(test richardson-lucy-recovers-some-of-a-known-blur
  "Deconvolving a blur by the PSF that made it must beat leaving it blurred."
  (im:with-image (source (binary-dumbbell :width 48 :height 48 :radius 9))
    ;; A binary image is one sample per pixel with values 0 and 1; promote it
    ;; to a gray byte image so there is a real intensity range to restore.
    (im:with-images ((truth (im:create 48 48 :color-space-gray :data-type-byte))
                     (blurred (im:create 48 48 :color-space-gray :data-type-byte))
                     (restored (im:create 48 48 :color-space-gray :data-type-byte))
                     (psf (im:create 9 9 :color-space-gray :data-type-float)))
      (dotimes (i (im:pixel-count source))
        (setf (cffi:mem-aref (im:plane-pointer truth 0) :unsigned-char i)
              (* 255 (cffi:mem-aref (im:plane-pointer source 0) :unsigned-char i))))
      (im:convolve-gaussian truth blurred 1.5d0)
      (im:render-gaussian psf 1.5d0)
      (im:deconvolve-richardson-lucy blurred psf restored :iterations 25)
      (is (< (im:rms-error truth restored) (im:rms-error truth blurred))
          "the restored image must be closer to the truth than the blurred one"))))

(test richardson-lucy-refuses-an-even-sided-psf
  "An even kernel has no centre, so the result would be shifted half a pixel.

IM refuses it by returning its cancellation value, which would otherwise
surface as IM:OPERATION-ABORTED -- a report that the user stopped the work."
  (im:with-image (source (gray-gradient 16 16))
    (im:with-images ((result (im:create-based source))
                     (even (im:create 4 4 :color-space-gray :data-type-float))
                     (odd (im:create 5 5 :color-space-gray :data-type-float)))
      (im:render-gaussian even 1.0d0)
      (im:render-gaussian odd 1.0d0)
      (signals im:data-error
        (im:deconvolve-richardson-lucy source even result :iterations 1))
      (signals im:data-error
        (im:deconvolve-richardson-lucy source odd result :iterations -1))
      (finishes (im:deconvolve-richardson-lucy source odd result :iterations 1)))))

;;; Shape and intensity measurement -------------------------------------------

(test measurements-describe-a-block-of-known-size
  "A solid 8x8 block at a known place, measured every way there is.

Every number here is decided by the fixture, which is the point: a
measurement that returned a plausible constant would pass a test that only
checked it was positive."
  (im:with-image (block-image (binary-block :width 32 :height 32 :x 8 :y 8 :size 8))
    (multiple-value-bind (labelled count) (im:find-regions block-image)
      (im:with-image (owned labelled)
        (is (= 1 count))
        (let ((box (aref (im:region-bounding-boxes owned count) 0))
              (hull (aref (im:region-convex-hulls owned count) 0))
              (feret (aref (im:region-feret-diameters owned count) 0)))
          (is (= 8 (getf box :xmin)))
          (is (= 15 (getf box :xmax)) "the box is inclusive, so 8 + 8 - 1")
          (is (= 8 (getf box :ymin)))
          (is (= 15 (getf box :ymax)))
          ;; The hull is a polygon through pixel CENTRES, so a solid 8x8 block
          ;; of 64 pixels has a hull of 7 by 7. This is why the CLI reports
          ;; hull area without deriving a solidity from it.
          (is (= 49.0d0 (getf hull :area)))
          (is (= 28.0d0 (getf hull :perimeter)))
          ;; Corner to corner of a 7 by 7 square.
          (is (< (abs (- (getf feret :max) (* 7 (sqrt 2.0d0)))) 0.001d0))
          (is (= 7.0d0 (getf feret :min))
              "the narrowest width of a square is its side")
          (is (<= 0 (getf feret :max-angle) 180))
          (is (<= 0 (getf feret :min-angle) 180)))))))

(test region-intensities-measure-the-second-image-not-the-labels
  "The one measurement that reads a second image, and says how bright a region is."
  (im:with-image (block-image (binary-block :width 32 :height 32 :x 8 :y 8 :size 8))
    (im:with-image (gray (im:create 32 32 :color-space-gray :data-type-byte))
      (set-pixels gray 0 10)
      ;; Paint the block's own pixels a constant 200, so the answer is known
      ;; exactly: mean 200, stddev 0, sum 64 * 200.
      (loop for y from 8 below 16
            do (loop for x from 8 below 16
                     do (setf (cffi:mem-aref (im:plane-pointer gray 0)
                                             :unsigned-char (+ (* y 32) x))
                              200)))
      (multiple-value-bind (labelled count) (im:find-regions block-image)
        (im:with-image (owned labelled)
          (let ((intensity (aref (im:region-intensities owned gray count) 0)))
            (is (= 200.0d0 (getf intensity :min)))
            (is (= 200.0d0 (getf intensity :max)))
            (is (= 200.0d0 (getf intensity :mean)))
            (is (= 0.0d0 (getf intensity :stddev)))
            (is (= 12800.0d0 (getf intensity :sum)) "64 pixels at 200")))))))

(test measurements-refuse-a-label-image-of-the-wrong-type
  "Every one of these C functions reports a bad label image as a cancellation.

IM documents IM_GRAY/IM_USHORT and returns its counter-abort value otherwise,
so without a check here `region-feret-diameters' on a byte image would signal
IM:OPERATION-ABORTED and blame the progress callback."
  (im:with-image (gray (gray-gradient 16 16))
    (signals im:data-error (im:region-bounding-boxes gray 1))
    (signals im:data-error (im:region-convex-hulls gray 1))
    (signals im:data-error (im:region-feret-diameters gray 1))
    (signals im:data-error (im:region-intensities gray gray 1))))

(test measurements-of-no-regions-are-an-empty-vector
  "An image with nothing in it measures to nothing, rather than allocating zero
bytes and reading them."
  (im:with-image (empty (im:create 16 16 :color-space-binary :data-type-byte))
    (set-pixels empty 0 0)
    (multiple-value-bind (labelled count) (im:find-regions empty)
      (im:with-image (owned labelled)
        (is (zerop count))
        (is (zerop (length (im:region-bounding-boxes owned count))))
        (is (zerop (length (im:region-feret-diameters owned count))))))))

(test region-labelling-reports-progress-and-does-not-crash
  "Labelling with a progress callback attached both works and reports.

Up to tecgraf-im v2.2.0 it did neither. imAnalyzeFindRegions opened its
counter with imCounterBegin and closed it with imProcessCounterEnd, which in
the OpenMP build freed an omp_lock_t the plain Begin never allocated, so the
process died at address 0 as the operation finished -- only with a callback
attached, which is why `im analyze' worked and `im analyze --verbose' did not.
This binding worked around it by detaching the callback around these two
calls, which bought safety at the price of the reports and of cancellation.

v2.2.1 fixes it in C and the workaround is gone, so the callback count is the
assertion that matters here: a nonzero count is the thing the workaround could
not deliver, and it is what says the upstream fix is present rather than
merely that nothing crashed."
  (im:with-image (dumbbell (binary-dumbbell))
    (im:with-images ((connected (im:make-label-image dumbbell))
                     (split (im:make-label-image dumbbell)))
      (let ((calls 0))
        (im:with-progress ((lambda (id text progress)
                             (declare (ignore id text progress))
                             (incf calls)
                             t))
          (is (= 1 (nth-value 1 (im:find-regions dumbbell connected))))
          (is (= 2 (nth-value 1 (im:watershed-segment dumbbell split)))))
        (is (plusp calls)
            "no progress was reported; the callback is being detached again")))))

(test a-short-region-count-measures-a-prefix
  "Asking for fewer regions than the image carries measures the first few.

It used to corrupt the heap. imAnalyzeMeasureArea scans the whole label image
and increments data_area[label-1] without range-checking the index, so a count
below the number of labels present wrote past the end of the array; this
binding scanned for the highest label and refused the call. tecgraf-im v2.2.1
range-checks in C, so the scan is gone and the call means what it reads as.

The arrays are exact-sized on purpose -- the failure this replaces was a write
one element past the end, and a generous buffer would hide it."
  (im:with-image (dumbbell (binary-dumbbell))
    (multiple-value-bind (labelled count) (im:watershed-segment dumbbell)
      (im:with-image (owned labelled)
        (is (= 2 count))
        (let ((all (im:region-areas owned 2))
              (prefix (im:region-areas owned 1))
              (centroids (im:region-centroids owned 1)))
          (is (= 1 (length prefix)))
          (is (= (aref all 0) (aref prefix 0))
              "the prefix must measure the same region, not a different one")
          (is (= 1 (length centroids)))
          ;; The first region of the dumbbell is the left disc, so its centroid
          ;; sits left of centre. A prefix that had measured both regions'
          ;; pixels into one slot would land near the middle.
          (is (< (car (aref centroids 0)) (/ (im:width owned) 2))))))))
