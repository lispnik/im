(defpackage #:im-process-cffi
  (:use #:common-lisp
        #:im-cffi)
  (:export #:counter-aborted))

(in-package #:im-process-cffi)

#+im-process-use-openmp
(cffi:define-foreign-library lib-im-process
  (:darwin "libim_process_omp.dylib")
  (:unix "libim_process_omp.so")
  (:windows "im_process_omp.dll")
  (t (:default "im_process_omp")))

#-im-process-use-openmp
(cffi:define-foreign-library lib-im-process
  (:darwin "libim_process.dylib")
  (:unix "libim_process.so")
  (:windows "im_process.dll")
  (t (:default "im_process")))

(cffi:use-foreign-library lib-im-process)

;; FFT/IFFT/Auto- and CrossCorrelation/AutoCovariance live in a
;; separate libim_fftw3 add-on. Try to load it; ignore the error if
;; absent (calls to those functions will then fail with
;; UNDEFINED-ALIEN-FUNCTION-ERROR).
(cffi:define-foreign-library lib-im-fftw3
  (:darwin "libim_fftw3.dylib")
  (:unix "libim_fftw3.so")
  (:windows "im_fftw3.dll")
  (t (:default "im_fftw3")))

(handler-case
    (cffi:use-foreign-library lib-im-fftw3)
  (cffi:load-foreign-library-error () nil))

(define-condition counter-aborted () ())

(defun counter-aborted-from-c (value)
  (unless value
    (signal 'counter-aborted)))

(cffi:defctype counter-aborted
    (:wrapper :boolean
     :from-c counter-aborted-from-c
     :to-c nil))

;;; im_process_ana.h

(cffi:defcfun (%im-calc-rms-error "imCalcRMSError") counter-aborted
  (im-image-1 im-image)
  (im-image-2 im-image)
  (rms-error-ptr (:pointer :double)))

(cffi:defcfun (%im-calc-snr "imCalcSNR") counter-aborted
  (src-im-image im-image)
  (noise-im-image im-image)
  (snr-ptr (:pointer :double)))

(cffi:defcfun (%im-calc-count-colors "imCalcCountColors") counter-aborted
  (im-image im-image)
  (count-ptr (:pointer :double)))

(cffi:defcfun (%im-calc-gray-histogram "imCalcGrayHistogram") counter-aborted
  (im-image im-image)
  (histo-ptr (:pointer :long))
  (cumulative :boolean))

(cffi:defcfun (%im-calc-histogram "imCalcHistogram") counter-aborted
  (im-image im-image)
  (histo-ptr (:pointer :long))
  (plane :int)
  (cumulative :boolean))

(cffi:defcfun (%im-calc-byte-histogram "imCalcByteHistogram") :void
  (data (:pointer :unsigned-char))
  (count :int)
  (histo-ptr (:pointer :long))
  (cumulative :boolean))

(cffi:defcfun (%im-calc-u-short-histogram "imCalcUShortHistogram") :void
  (data (:pointer :unsigned-short))
  (count :int)
  (histo-ptr (:pointer :long))
  (cumulative :boolean))

(cffi:defcfun (%im-calc-short-histogram "imCalcShortHistogram") :void
  (data (:pointer :short))
  (count :int)
  (histo-ptr (:pointer :long))
  (cumulative :boolean))

(cffi:defcfun (%im-histogram-new "imHistogramNew") (:pointer :unsigned-long)
  (data-type im-cffi::data-type)
  (hcount-ptr (:pointer :int)))

(cffi:defcfun (%im-histogram-release "imHistogramRelease") :void
  (histo-ptr (:pointer :unsigned-long)))

(cffi:defcfun (%im-histogram-shift "imHistogramShift") :int
  (data-type im-cffi::data-type))

(cffi:defcfun (%im-histogram-count "imHistogramCount") :int
  (data-type im-cffi::data-type))

(cffi:defcstruct im-stats-struct
  (max :double)
  (min :double)
  (positive :unsigned-long)
  (negative :unsigned-long)
  (zeros :unsigned-long)
  (mean :double)
  (stddev :double))

(cffi:defctype im-stats (:pointer (:struct im-stats-struct)))

(cffi:defcfun (%im-calc-image-statistics "imCalcImageStatistics") counter-aborted
  (image im-image)
  (stats im-stats))

(cffi:defcfun (%im-calc-histogram-statistics "imCalcHistogramStatistics") counter-aborted
  (image im-image)
  (stats im-stats))

(cffi:defcfun (%im-calc-histo-image-statistics "imCalcHistoImageStatistics") counter-aborted
  (image im-image)
  (median (:pointer :int))
  (mode (:pointer :int)))

(cffi:defcfun (%im-calc-percent-min-max "imCalcPercentMinMax") counter-aborted
  (image im-image)
  (percent :double)
  (ignore-zero :boolean)
  (min (:pointer :int))
  (max (:pointer :int)))

;;; im_process_glo.h

;; int imProcessHoughLines(const imImage* src_image, imImage* dst_image);
;; int imProcessHoughLinesDraw(const imImage* src_image, const imImage* hough, const imImage* hough_points, imImage* dst_image);
;; void imProcessCrossCorrelation(const imImage* src_image1, const imImage* src_image2, imImage* dst_image);
;; void imProcessAutoCorrelation(const imImage* src_image, imImage* dst_image);
;; void imProcessDistanceTransform(const imImage* src_image, imImage* dst_image);
;; void imProcessRegionalMaximum(const imImage* src_image, imImage* dst_image);
;; void imProcessFFT(const imImage* src_image, imImage* dst_image);
;; void imProcessIFFT(const imImage* src_image, imImage* dst_image);
;; void imProcessFFTraw(imImage* image, int inverse, int center, int normalize);
;; void imProcessSwapQuadrants(imImage* image, int center2origin);

(cffi:defcfun (%im-process-openmp-set-min-count "imProcessOpenMPSetMinCount") :int
  (min-count :int))

(cffi:defcfun (%im-process-openmp-set-num-threads "imProcessOpenMPSetNumThreads") :int
  (count :int))

(cffi:defcfun (%im-gaussian-stddev-to-kernel-size "imGaussianStdDev2KernelSize") :int
  (stddev :double))

(cffi:defcfun (%im-gaussian-kernel-size-to-stddev "imGaussianKernelSize2StdDev") :double
  (kernel-size :int))

;;; im_process_loc.h

(cffi:defcfun (%im-process-calc-rotate-size "imProcessCalcRotateSize") :void
  (width :int)
  (height :int)
  (new-width-ptr (:pointer :int))
  (new-height-ptr (:pointer :int))
  (cos0 :double)
  (sin0 :double))

(cffi:defcfun (%im-process-rotate "imProcessRotate") counter-aborted
  (src-image im-image)
  (dst-image im-image)
  (cos0 :double)
  (sin0 :double)
  (order :int))

(cffi:defcfun (%im-process-rotate-ref "imProcessRotateRef") counter-aborted
  (src-image im-image)
  (dst-image im-image)
  (cos0 :double)
  (sin0 :double)
  (x :int)
  (y :int)
  (to-origin :boolean)
  (order :int))

(cffi:defcfun (%im-process-rotate-90 "imProcessRotate90") counter-aborted
  (src-image im-image)
  (dst-image im-image)
  (dir-clockwise :int))

(cffi:defcfun (%im-process-rotate-180 "imProcessRotate180") counter-aborted
  (src-image im-image)
  (dst-image im-image))

(cffi:defcfun (%im-process-mirror "imProcessMirror") counter-aborted
  (src-image im-image)
  (dst-image im-image))

(cffi:defcfun (%im-process-flip "imProcessFlip") counter-aborted
  (src-image im-image)
  (dst-image im-image))

(cffi:defcfun (%im-process-radial "imProcessRadial") counter-aborted
  (src-image im-image)
  (dst-image im-image)
  (k1 :double)
  (order :int))

(cffi:defcfun (%im-process-lens-distort "imProcessLensDistort") counter-aborted
  (src-image im-image)
  (dst-image im-image)
  (a :double)
  (b :double)
  (c :double)
  (order :int))

(cffi:defcfun (%im-process-swirl "imProcessSwirl") counter-aborted
  (src-image im-image)
  (dst-image im-image)
  (k1 :double)
  (order :int))

(cffi:defcfun (%im-process-interlace-split "imProcessInterlaceSplit") counter-aborted
  (src-image im-image)
  (dst-image1 im-image)
  (dst-image2 im-image))

(cffi:defcfun (%im-process-gray-morph-convolve "imProcessGrayMorphConvolve") counter-aborted
  (src-im-image im-image)
  (dst-im-image im-image)
  (kernal im-image)
  (is-max :boolean))

(cffi:defcfun (%im-process-gray-morph-erode "imProcessGrayMorphErode") counter-aborted
  (src-im-image im-image)
  (dst-im-image im-image)
  (kernel-size :int))

(cffi:defcfun (%im-process-gray-morph-dilate "imProcessGrayMorphDilate") counter-aborted
  (src-im-image im-image)
  (dst-im-image im-image)
  (kernel-size :int))

(cffi:defcfun (%im-process-gray-morph-open "imProcessGrayMorphOpen") counter-aborted
  (src-im-image im-image)
  (dst-im-image im-image)
  (kernel-size :int))

(cffi:defcfun (%im-process-gray-morph-close "imProcessGrayMorphClose") counter-aborted
  (src-im-image im-image)
  (dst-im-image im-image)
  (kernel-size :int))

(cffi:defcfun (%im-process-gray-morph-top-hat "imProcessGrayMorphTopHat") counter-aborted
  (src-im-image im-image)
  (dst-im-image im-image)
  (kernel-size :int))

(cffi:defcfun (%im-process-gray-morph-well "imProcessGrayMorphWell") counter-aborted
  (src-im-image im-image)
  (dst-im-image im-image)
  (kernel-size :int))

(cffi:defcfun (%im-process-gray-morph-gradient "imProcessGrayMorphGradient") counter-aborted
  (src-im-image im-image)
  (dst-im-image im-image)
  (kernel-size :int))

(cffi:defcfun (%im-process-bin-morph-convolve "imProcessBinMorphConvolve") counter-aborted
  (src-im-image im-image)
  (dst-im-image im-image)
  (kernel im-image)
  (hit-white :boolean)
  (iter :int))

(cffi:defcfun (%im-process-bin-morph-erode "imProcessBinMorphErode") counter-aborted
  (src-im-image im-image)
  (dst-im-image im-image)
  (kernel-size :int)
  (iter :int))

(cffi:defcfun (%im-process-bin-morph-dilate "imProcessBinMorphDilate") counter-aborted
  (src-im-image im-image)
  (dst-im-image im-image)
  (kernel-size :int)
  (iter :int))

(cffi:defcfun (%im-process-bin-morph-open "imProcessBinMorphOpen") counter-aborted
  (src-im-image im-image)
  (dst-im-image im-image)
  (kernel-size :int)
  (iter :int))

(cffi:defcfun (%im-process-bin-morph-close "imProcessBinMorphClose") counter-aborted
  (src-im-image im-image)
  (dst-im-image im-image)
  (kernel-size :int)
  (iter :int))

(cffi:defcfun (%im-process-bin-morph-outline "imProcessBinMorphOutline") counter-aborted
  (src-im-image im-image)
  (dst-im-image im-image)
  (kernel-size :int)
  (iter :int))

(cffi:defcfun (%im-process-bin-morph-thin "imProcessBinMorphThin") counter-aborted
  (src-im-image im-image)
  (dst-im-image im-image))

;;; im_process_pnt.h

;; ... TODO

(cffi:defctype im-render-func :pointer)
(cffi:defctype im-render-cond-func :pointer)

(cffi:defcfun (%im-process-render-op "imProcessRenderOp") counter-aborted
  (im-image im-image)
  (func im-render-func)
  (render-name :string)
  (params (:pointer :double))
  (plus :boolean))

(cffi:defcfun (%im-process-render-cond-op "imProcessRenderCondOp") counter-aborted
  (im-image im-image)
  (func im-render-cond-func)
  (render-name :string)
  (params (:pointer :double)))

(cffi:defcfun (%im-process-render-add-speckle-noise "imProcessRenderAddSpeckleNoise") counter-aborted
  (src-im-image im-image)
  (dst-im-image im-image)
  (percent :double))

(cffi:defcfun (%im-process-render-add-gaussian-noise "imProcessRenderAddGaussianNoise") counter-aborted
  (src-im-image im-image)
  (dst-im-image im-image)
  (mean :double)
  (std-dev :double))

(cffi:defcfun (%im-process-render-add-uniform-noise "imProcessRenderAddUniformNoise") counter-aborted
  (src-im-image im-image)
  (dst-im-image im-image)
  (mean :double)
  (std-dev :double))

(cffi:defcfun (%im-process-render-random-noise "imProcessRenderRandomNoise") counter-aborted
  (im-image im-image))

(cffi:defcfun (%im-process-render-constant "imProcessRenderConstant") counter-aborted
  (im-image im-image)
  (value (:pointer :double)))

(cffi:defcfun (%im-process-render-wheel "imProcessRenderWheel") counter-aborted
  (im-image im-image)
  (internal-radius :int)
  (external-radius :int))

(cffi:defcfun (%im-process-render-tent "imProcessRenderTent") counter-aborted
  (im-image im-image)
  (tent-width :int)
  (tent-heoght :int))

(cffi:defcfun (%im-process-render-ramp "imProcessRenderRamp") counter-aborted
  (im-image im-image)
  (start :int)
  (end :int)
  (vertical-direction-p :boolean))

(cffi:defcfun (%im-process-render-box "imProcessRenderBox") counter-aborted
  (im-image im-image)
  (width :int)
  (height :int))

(cffi:defcfun (%im-process-render-sinc "imProcessRenderSinc") counter-aborted
  (im-image im-image)
  (x-period :double)
  (y-period :double))

(cffi:defcfun (%im-process-render-gaussian "imProcessRenderGaussian") counter-aborted
  (im-image im-image)
  (std-dev :double))

(cffi:defcfun (%im-process-render-lap-of-gaussian "imProcessRenderLapOfGaussian") counter-aborted
  (im-image im-image)
  (std-dev :double))

(cffi:defcfun (%im-process-render-cosine "imProcessRenderCosine") counter-aborted
  (im-image im-image)
  (x-period :double)
  (y-period :double))

(cffi:defcfun (%im-process-render-grid "imProcessRenderGrid") counter-aborted
  (im-image im-image)
  (x-space :double)
  (y-space :double))

(cffi:defcfun (%im-process-render-chessboard "imProcessRenderChessboard") counter-aborted
  (im-image im-image)
  (x-space :double)
  (y-space :double))

(cffi:defcfun (%im-process-render-cone "imProcessRenderCone") counter-aborted
  (im-image im-image)
  (radius :int))

(cffi:defcfun (%im-process-render-flood-fill "imProcessRenderFloodFill") :void
  (im-image im-image)
  (start-x :int)
  (start-y :int)
  (replace-color (:pointer :double))
  (tolerance :double))

(cffi:defcfun (%im-process-render-op-alpha "imProcessRenderOpAlpha") counter-aborted
  (im-image im-image)
  (func im-render-func)
  (render-name :string)
  (params (:pointer :double))
  (plus :boolean))

(cffi:defcfun (%im-process-render-cond-op-alpha "imProcessRenderCondOpAlpha") counter-aborted
  (im-image im-image)
  (func im-render-cond-func)
  (render-name :string)
  (params (:pointer :double)))

;;; im_process_pnt.h enums

(cffi:defcenum unary-op
  :unary-op-equal
  :unary-op-absolute
  :unary-op-less
  :unary-op-invert
  :unary-op-square
  :unary-op-square-root
  :unary-op-log
  :unary-op-exp
  :unary-op-sin
  :unary-op-cos
  :unary-op-conjugate
  :unary-op-cpx-norm
  :unary-op-positives
  :unary-op-negatives)

(cffi:defcenum binary-op
  :binary-op-add
  :binary-op-sub
  :binary-op-mul
  :binary-op-div
  :binary-op-diff
  :binary-op-pow
  :binary-op-min
  :binary-op-max)

(cffi:defcenum bitwise-op
  :bitwise-op-and
  :bitwise-op-or
  :bitwise-op-xor)

(cffi:defcenum tone-gamut-op
  :tone-gamut-normalize
  :tone-gamut-pow
  :tone-gamut-log
  :tone-gamut-exp
  :tone-gamut-invert
  :tone-gamut-zero-start
  :tone-gamut-solarize
  :tone-gamut-slice
  :tone-gamut-expand
  :tone-gamut-crop
  :tone-gamut-bright-cont)

;;; im_process_ana.h - Analyze

(cffi:defcfun (%im-analyze-find-regions "imAnalyzeFindRegions") counter-aborted
  (src-image im-image)
  (dst-image im-image)
  (connect :int)
  (touch-border :boolean)
  (region-count-ptr (:pointer :int)))

(cffi:defcfun (%im-analyze-measure-area "imAnalyzeMeasureArea") counter-aborted
  (image im-image)
  (area-ptr (:pointer :int))
  (region-count :int))

(cffi:defcfun (%im-analyze-measure-perim-area "imAnalyzeMeasurePerimArea") counter-aborted
  (image im-image)
  (perim-area-ptr (:pointer :double))
  (region-count :int))

(cffi:defcfun (%im-analyze-measure-centroid "imAnalyzeMeasureCentroid") counter-aborted
  (image im-image)
  (area-ptr (:pointer :int))
  (region-count :int)
  (cx-ptr (:pointer :double))
  (cy-ptr (:pointer :double)))

(cffi:defcfun (%im-analyze-measure-principal-axis "imAnalyzeMeasurePrincipalAxis") counter-aborted
  (image im-image)
  (area-ptr (:pointer :int))
  (cx-ptr (:pointer :double))
  (cy-ptr (:pointer :double))
  (region-count :int)
  (major-slope-ptr (:pointer :double))
  (major-length-ptr (:pointer :double))
  (minor-slope-ptr (:pointer :double))
  (minor-length-ptr (:pointer :double)))

(cffi:defcfun (%im-analyze-measure-holes "imAnalyzeMeasureHoles") counter-aborted
  (image im-image)
  (connect :int)
  (region-count :int)
  (holes-count-ptr (:pointer :int))
  (holes-area-ptr (:pointer :int))
  (holes-perim-ptr (:pointer :double)))

(cffi:defcfun (%im-analyze-measure-perimeter "imAnalyzeMeasurePerimeter") counter-aborted
  (image im-image)
  (perim-ptr (:pointer :double))
  (region-count :int))

(cffi:defcfun (%im-process-perimeter-line "imProcessPerimeterLine") counter-aborted
  (src-image im-image)
  (dst-image im-image))

(cffi:defcfun (%im-process-fill-holes "imProcessFillHoles") counter-aborted
  (src-image im-image)
  (dst-image im-image)
  (connect :int))

(cffi:defcfun (%im-process-remove-by-area "imProcessRemoveByArea") counter-aborted
  (src-image im-image)
  (dst-image im-image)
  (connect :int)
  (start-size :int)
  (end-size :int)
  (inside :boolean))

;;; im_process_glo.h - Global ops

(cffi:defcfun (%im-process-hough-lines "imProcessHoughLines") counter-aborted
  (src-image im-image)
  (dst-image im-image))

(cffi:defcfun (%im-process-hough-lines-draw "imProcessHoughLinesDraw") counter-aborted
  (src-image im-image)
  (hough im-image)
  (hough-points im-image)
  (dst-image im-image))

(cffi:defcfun (%im-process-cross-correlation "imProcessCrossCorrelation") :void
  (src-image1 im-image)
  (src-image2 im-image)
  (dst-image im-image))

(cffi:defcfun (%im-process-auto-correlation "imProcessAutoCorrelation") :void
  (src-image im-image)
  (dst-image im-image))

(cffi:defcfun (%im-process-distance-transform "imProcessDistanceTransform") :void
  (src-image im-image)
  (dst-image im-image))

(cffi:defcfun (%im-process-regional-maximum "imProcessRegionalMaximum") :void
  (src-image im-image)
  (dst-image im-image))

(cffi:defcfun (%im-process-fft "imProcessFFT") :void
  (src-image im-image)
  (dst-image im-image))

(cffi:defcfun (%im-process-ifft "imProcessIFFT") :void
  (src-image im-image)
  (dst-image im-image))

(cffi:defcfun (%im-process-fft-raw "imProcessFFTraw") :void
  (im-image im-image)
  (inverse :int)
  (center :int)
  (normalize :int))

(cffi:defcfun (%im-process-swap-quadrants "imProcessSwapQuadrants") :void
  (im-image im-image)
  (center-to-origin :int))

;;; im_process_loc.h - Convolve family

(cffi:defcfun (%im-process-convolve "imProcessConvolve") counter-aborted
  (src-image im-image)
  (dst-image im-image)
  (kernel im-image))

(cffi:defcfun (%im-process-convolve-sep "imProcessConvolveSep") counter-aborted
  (src-image im-image)
  (dst-image im-image)
  (kernel im-image))

(cffi:defcfun (%im-process-convolve-dual "imProcessConvolveDual") counter-aborted
  (src-image im-image)
  (dst-image im-image)
  (kernel1 im-image)
  (kernel2 im-image))

(cffi:defcfun (%im-process-convolve-rep "imProcessConvolveRep") counter-aborted
  (src-image im-image)
  (dst-image im-image)
  (kernel im-image)
  (count :int))

(cffi:defcfun (%im-process-compass-convolve "imProcessCompassConvolve") counter-aborted
  (src-image im-image)
  (dst-image im-image)
  (kernel im-image))

(cffi:defcfun (%im-process-rotate-kernel "imProcessRotateKernel") :void
  (kernel im-image))

(cffi:defcfun (%im-process-diff-of-gaussian-convolve "imProcessDiffOfGaussianConvolve") counter-aborted
  (src-image im-image)
  (dst-image im-image)
  (stddev1 :double)
  (stddev2 :double))

(cffi:defcfun (%im-process-lap-of-gaussian-convolve "imProcessLapOfGaussianConvolve") counter-aborted
  (src-image im-image)
  (dst-image im-image)
  (stddev :double))

(cffi:defcfun (%im-process-mean-convolve "imProcessMeanConvolve") counter-aborted
  (src-image im-image)
  (dst-image im-image)
  (kernel-size :int))

(cffi:defcfun (%im-process-gaussian-convolve "imProcessGaussianConvolve") counter-aborted
  (src-image im-image)
  (dst-image im-image)
  (stddev :double))

(cffi:defcfun (%im-process-barlett-convolve "imProcessBarlettConvolve") counter-aborted
  (src-image im-image)
  (dst-image im-image)
  (kernel-size :int))

(cffi:defcfun (%im-process-sobel-convolve "imProcessSobelConvolve") counter-aborted
  (src-image im-image)
  (dst-image im-image))

(cffi:defcfun (%im-process-prewitt-convolve "imProcessPrewittConvolve") counter-aborted
  (src-image im-image)
  (dst-image im-image))

(cffi:defcfun (%im-process-spline-edge-convolve "imProcessSplineEdgeConvolve") counter-aborted
  (src-image im-image)
  (dst-image im-image))

(cffi:defcfun (%im-process-zero-crossing "imProcessZeroCrossing") counter-aborted
  (src-image im-image)
  (dst-image im-image))

(cffi:defcfun (%im-process-canny "imProcessCanny") counter-aborted
  (src-image im-image)
  (dst-image im-image)
  (stddev :double))

(cffi:defcfun (%im-process-median-convolve "imProcessMedianConvolve") counter-aborted
  (src-image im-image)
  (dst-image im-image)
  (kernel-size :int))

(cffi:defcfun (%im-process-range-convolve "imProcessRangeConvolve") counter-aborted
  (src-image im-image)
  (dst-image im-image)
  (kernel-size :int))

(cffi:defcfun (%im-process-rank-closest-convolve "imProcessRankClosestConvolve") counter-aborted
  (src-image im-image)
  (dst-image im-image)
  (kernel-size :int))

(cffi:defcfun (%im-process-rank-max-convolve "imProcessRankMaxConvolve") counter-aborted
  (src-image im-image)
  (dst-image im-image)
  (kernel-size :int))

(cffi:defcfun (%im-process-rank-min-convolve "imProcessRankMinConvolve") counter-aborted
  (src-image im-image)
  (dst-image im-image)
  (kernel-size :int))

(cffi:defcfun (%im-process-unsharp "imProcessUnsharp") counter-aborted
  (src-image im-image)
  (dst-image im-image)
  (stddev :double)
  (amount :double)
  (threshold :double))

(cffi:defcfun (%im-process-sharp "imProcessSharp") counter-aborted
  (src-image im-image)
  (dst-image im-image)
  (amount :double)
  (threshold :double))

(cffi:defcfun (%im-process-sharp-kernel "imProcessSharpKernel") counter-aborted
  (src-image im-image)
  (kernel im-image)
  (dst-image im-image)
  (amount :double)
  (threshold :double))

;;; im_process_loc.h - Geometric

(cffi:defcfun (%im-process-resize "imProcessResize") counter-aborted
  (src-image im-image)
  (dst-image im-image)
  (order :int))

(cffi:defcfun (%im-process-reduce "imProcessReduce") counter-aborted
  (src-image im-image)
  (dst-image im-image)
  (order :int))

(cffi:defcfun (%im-process-reduce-by-4 "imProcessReduceBy4") counter-aborted
  (src-image im-image)
  (dst-image im-image))

(cffi:defcfun (%im-process-crop "imProcessCrop") counter-aborted
  (src-image im-image)
  (dst-image im-image)
  (xmin :int)
  (ymin :int))

(cffi:defcfun (%im-process-add-margins "imProcessAddMargins") counter-aborted
  (src-image im-image)
  (dst-image im-image)
  (xmin :int)
  (ymin :int))

(cffi:defcfun (%im-process-insert "imProcessInsert") counter-aborted
  (src-image im-image)
  (region-image im-image)
  (dst-image im-image)
  (xmin :int)
  (ymin :int))

;;; im_process_loc.h - Bin morph extra

(cffi:defcfun (%im-process-bin-thin-zhang-suen "imProcessBinThinZhangSuen") counter-aborted
  (src-image im-image)
  (dst-image im-image))

(cffi:defcfun (%im-process-bin-thin-nh-maps "imProcessBinThinNhMaps") counter-aborted
  (src-image im-image)
  (dst-image im-image))

;;; im_process_pnt.h - Custom point ops

(cffi:defctype unary-point-op-func :pointer)
(cffi:defctype unary-point-color-op-func :pointer)
(cffi:defctype multi-point-op-func :pointer)
(cffi:defctype multi-point-color-op-func :pointer)

(cffi:defcfun (%im-process-unary-point-op "imProcessUnaryPointOp") counter-aborted
  (src-image im-image)
  (dst-image im-image)
  (func unary-point-op-func)
  (params (:pointer :double))
  (userdata :pointer)
  (op-name :string))

(cffi:defcfun (%im-process-unary-point-color-op "imProcessUnaryPointColorOp") counter-aborted
  (src-image im-image)
  (dst-image im-image)
  (func unary-point-color-op-func)
  (params (:pointer :double))
  (userdata :pointer)
  (op-name :string))

(cffi:defcfun (%im-process-multi-point-op "imProcessMultiPointOp") counter-aborted
  (src-image-list (:pointer im-image))
  (src-image-count :int)
  (dst-image im-image)
  (func multi-point-op-func)
  (params (:pointer :double))
  (userdata :pointer)
  (op-name :string))

(cffi:defcfun (%im-process-multi-point-color-op "imProcessMultiPointColorOp") counter-aborted
  (src-image-list (:pointer im-image))
  (src-image-count :int)
  (dst-image im-image)
  (func multi-point-color-op-func)
  (params (:pointer :double))
  (userdata :pointer)
  (op-name :string))

;;; im_process_pnt.h - Arithmetic

(cffi:defcfun (%im-process-un-arithmetic-op "imProcessUnArithmeticOp") :void
  (src-image im-image)
  (dst-image im-image)
  (op unary-op))

(cffi:defcfun (%im-process-arithmetic-op "imProcessArithmeticOp") :void
  (src-image1 im-image)
  (src-image2 im-image)
  (dst-image im-image)
  (op binary-op))

(cffi:defcfun (%im-process-arithmetic-const-op "imProcessArithmeticConstOp") :void
  (src-image im-image)
  (src-const :double)
  (dst-image im-image)
  (op binary-op))

(cffi:defcfun (%im-process-blend-const "imProcessBlendConst") :void
  (src-image1 im-image)
  (src-image2 im-image)
  (dst-image im-image)
  (alpha :double))

(cffi:defcfun (%im-process-blend "imProcessBlend") :void
  (src-image1 im-image)
  (src-image2 im-image)
  (alpha-image im-image)
  (dst-image im-image))

(cffi:defcfun (%im-process-compose "imProcessCompose") :void
  (src-image1 im-image)
  (src-image2 im-image)
  (dst-image im-image))

(cffi:defcfun (%im-process-split-complex "imProcessSplitComplex") :void
  (src-image im-image)
  (dst-image1 im-image)
  (dst-image2 im-image)
  (polar :int))

(cffi:defcfun (%im-process-merge-complex "imProcessMergeComplex") :void
  (src-image1 im-image)
  (src-image2 im-image)
  (dst-image im-image)
  (polar :int))

(cffi:defcfun (%im-process-multiply-conj "imProcessMultiplyConj") :void
  (src-image1 im-image)
  (src-image2 im-image)
  (dst-image im-image))

(cffi:defcfun (%im-process-multiple-mean "imProcessMultipleMean") counter-aborted
  (src-image-list (:pointer im-image))
  (src-image-count :int)
  (dst-image im-image))

(cffi:defcfun (%im-process-multiple-std-dev "imProcessMultipleStdDev") counter-aborted
  (src-image-list (:pointer im-image))
  (src-image-count :int)
  (mean-image im-image)
  (dst-image im-image))

(cffi:defcfun (%im-process-multiple-median "imProcessMultipleMedian") counter-aborted
  (src-image-list (:pointer im-image))
  (src-image-count :int)
  (dst-image im-image))

(cffi:defcfun (%im-process-auto-covariance "imProcessAutoCovariance") counter-aborted
  (src-image im-image)
  (mean-image im-image)
  (dst-image im-image))

(cffi:defcfun (%im-process-back-sub "imProcessBackSub") counter-aborted
  (src-image1 im-image)
  (src-image2 im-image)
  (dst-image im-image)
  (tol :double)
  (show-diff :int))

;;; im_process_pnt.h - Quantize / histogram

(cffi:defcfun (%im-process-quantize-rgb-uniform "imProcessQuantizeRGBUniform") :void
  (src-image im-image)
  (dst-image im-image)
  (do-dither :boolean))

(cffi:defcfun (%im-process-quantize-rgb-median-cut "imProcessQuantizeRGBMedianCut") :void
  (src-image im-image)
  (dst-image im-image))

(cffi:defcfun (%im-process-quantize-gray-uniform "imProcessQuantizeGrayUniform") :void
  (src-image im-image)
  (dst-image im-image)
  (grays :int))

(cffi:defcfun (%im-process-quantize-gray-median-cut "imProcessQuantizeGrayMedianCut") :void
  (src-image im-image)
  (dst-image im-image)
  (grays :int))

(cffi:defcfun (%im-process-expand-histogram "imProcessExpandHistogram") :void
  (src-image im-image)
  (dst-image im-image)
  (percent :double))

(cffi:defcfun (%im-process-equalize-histogram "imProcessEqualizeHistogram") :void
  (src-image im-image)
  (dst-image im-image))

;;; im_process_pnt.h - Component split / merge / color

(cffi:defcfun (%im-process-split-y-chroma "imProcessSplitYChroma") :void
  (src-image im-image)
  (y-image im-image)
  (chroma-image im-image))

(cffi:defcfun (%im-process-split-hsi "imProcessSplitHSI") :void
  (src-image im-image)
  (h-image im-image)
  (s-image im-image)
  (i-image im-image))

(cffi:defcfun (%im-process-merge-hsi "imProcessMergeHSI") :void
  (h-image im-image)
  (s-image im-image)
  (i-image im-image)
  (dst-image im-image))

(cffi:defcfun (%im-process-split-components "imProcessSplitComponents") :void
  (src-image im-image)
  (dst-image-list (:pointer im-image)))

(cffi:defcfun (%im-process-merge-components "imProcessMergeComponents") :void
  (src-image-list (:pointer im-image))
  (dst-image im-image))

(cffi:defcfun (%im-process-normalize-components "imProcessNormalizeComponents") :void
  (src-image im-image)
  (dst-image im-image))

(cffi:defcfun (%im-process-replace-color "imProcessReplaceColor") :void
  (src-image im-image)
  (dst-image im-image)
  (src-color (:pointer :double))
  (dst-color (:pointer :double)))

(cffi:defcfun (%im-process-set-alpha-color "imProcessSetAlphaColor") :void
  (src-image im-image)
  (dst-image im-image)
  (src-color (:pointer :double))
  (dst-alpha :double))

(cffi:defcfun (%im-process-pseudo-color "imProcessPseudoColor") :void
  (src-image im-image)
  (dst-image im-image))

(cffi:defcfun (%im-process-fix-bgr "imProcessFixBGR") :void
  (src-image im-image)
  (dst-image im-image))

(cffi:defcfun (%im-process-select-hue "imProcessSelectHue") :void
  (src-image im-image)
  (dst-image im-image)
  (hue-start :double)
  (hue-end :double))

(cffi:defcfun (%im-process-select-hsi "imProcessSelectHSI") :void
  (src-image im-image)
  (dst-image im-image)
  (hue-start :double)
  (hue-end :double)
  (sat-start :double)
  (sat-end :double)
  (int-start :double)
  (int-end :double))

(cffi:defcfun (%im-process-shift-hsi "imProcessShiftHSI") :void
  (src-image im-image)
  (dst-image im-image)
  (h-shift :double)
  (s-shift :double)
  (i-shift :double))

(cffi:defcfun (%im-process-shift-component "imProcessShiftComponent") :void
  (src-image im-image)
  (dst-image im-image)
  (c0-shift :double)
  (c1-shift :double)
  (c2-shift :double))

(cffi:defcfun (%im-process-tone-gamut "imProcessToneGamut") :void
  (src-image im-image)
  (dst-image im-image)
  (op tone-gamut-op)
  (params (:pointer :double)))

(cffi:defcfun (%im-process-un-normalize "imProcessUnNormalize") :void
  (src-image im-image)
  (dst-image im-image))

(cffi:defcfun (%im-process-direct-conv "imProcessDirectConv") :void
  (src-image im-image)
  (dst-image im-image))

(cffi:defcfun (%im-process-negative "imProcessNegative") :void
  (src-image im-image)
  (dst-image im-image))

(cffi:defcfun (%im-process-calc-auto-gamma "imProcessCalcAutoGamma") :double
  (image im-image))

(cffi:defcfun (%im-process-pixelate "imProcessPixelate") :void
  (src-image im-image)
  (dst-image im-image)
  (box-size :int))

(cffi:defcfun (%im-process-posterize "imProcessPosterize") :void
  (src-image im-image)
  (dst-image im-image)
  (level :int))

;;; im_process_pnt.h - Threshold

(cffi:defcfun (%im-process-threshold "imProcessThreshold") :void
  (src-image im-image)
  (dst-image im-image)
  (level :double)
  (value :int))

(cffi:defcfun (%im-process-threshold-by-diff "imProcessThresholdByDiff") :void
  (src-image1 im-image)
  (src-image2 im-image)
  (dst-image im-image))

(cffi:defcfun (%im-process-hysteresis-threshold "imProcessHysteresisThreshold") :void
  (src-image im-image)
  (dst-image im-image)
  (low-thres :int)
  (high-thres :int))

(cffi:defcfun (%im-process-hysteresis-thres-estimate "imProcessHysteresisThresEstimate") :void
  (image im-image)
  (low-level (:pointer :int))
  (high-level (:pointer :int)))

(cffi:defcfun (%im-process-uniform-err-threshold "imProcessUniformErrThreshold") counter-aborted
  (src-image im-image)
  (dst-image im-image))

(cffi:defcfun (%im-process-diffusion-err-threshold "imProcessDiffusionErrThreshold") :void
  (src-image im-image)
  (dst-image im-image)
  (level :int))

(cffi:defcfun (%im-process-percent-threshold "imProcessPercentThreshold") counter-aborted
  (src-image im-image)
  (dst-image im-image)
  (percent :double))

(cffi:defcfun (%im-process-otsu-threshold "imProcessOtsuThreshold") :int
  (src-image im-image)
  (dst-image im-image))

(cffi:defcfun (%im-process-min-max-threshold "imProcessMinMaxThreshold") :double
  (src-image im-image)
  (dst-image im-image))

(cffi:defcfun (%im-process-local-max-thres-estimate "imProcessLocalMaxThresEstimate") :void
  (image im-image)
  (level (:pointer :int)))

(cffi:defcfun (%im-process-local-max-threshold "imProcessLocalMaxThreshold") counter-aborted
  (src-image im-image)
  (dst-image im-image)
  (kernel-size :int)
  (min-level :int))

(cffi:defcfun (%im-process-range-contrast-threshold "imProcessRangeContrastThreshold") counter-aborted
  (src-image im-image)
  (dst-image im-image)
  (kernel-size :int)
  (min-range :int))

(cffi:defcfun (%im-process-slice-threshold "imProcessSliceThreshold") :void
  (src-image im-image)
  (dst-image im-image)
  (start-level :double)
  (end-level :double))

(cffi:defcfun (%im-process-threshold-color "imProcessThresholdColor") :void
  (src-image im-image)
  (dst-image im-image)
  (src-color (:pointer :double))
  (tol :double))

(cffi:defcfun (%im-process-threshold-saturation "imProcessThresholdSaturation") :void
  (src-image im-image)
  (dst-image im-image)
  (s-min :double))

;;; im_process_pnt.h - Bitwise

(cffi:defcfun (%im-process-bitwise-op "imProcessBitwiseOp") :void
  (src-image1 im-image)
  (src-image2 im-image)
  (dst-image im-image)
  (op bitwise-op))

(cffi:defcfun (%im-process-bitwise-not "imProcessBitwiseNot") :void
  (src-image im-image)
  (dst-image im-image))

(cffi:defcfun (%im-process-bit-mask "imProcessBitMask") :void
  (src-image im-image)
  (dst-image im-image)
  (mask :unsigned-char)
  (op bitwise-op))

(cffi:defcfun (%im-process-bit-plane "imProcessBitPlane") :void
  (src-image im-image)
  (dst-image im-image)
  (plane :int)
  (do-reset :boolean))

(cffi:defcfun (%im-process-binary-mask "imProcessBinaryMask") :void
  (src-image im-image)
  (dst-image im-image)
  (mask-image im-image))

;;; im_process_pnt.h - Convert (driven through process counter)

(cffi:defcfun (%im-process-convert-color-space "imProcessConvertColorSpace") :int
  (src-image im-image)
  (dst-image im-image))

(cffi:defcfun (%im-process-convert-data-type "imProcessConvertDataType") :int
  (src-image im-image)
  (dst-image im-image)
  (cpx2real :int)
  (gamma :double)
  (absolute :boolean)
  (cast-mode :int))

(cffi:defcfun (%im-process-convert-to-bitmap "imProcessConvertToBitmap") :int
  (src-image im-image)
  (dst-image im-image)
  (cpx2real :int)
  (gamma :double)
  (absolute :boolean)
  (cast-mode :int))

;;; im_process_pon.h - Hyperion / specialised

(cffi:defcfun (%im-process-abnormal-hyperion-correction "imProcessAbnormalHyperionCorrection") :void
  (src-image im-image)
  (dst-image im-image)
  (threshold :int)
  (channel :int)
  (image-list (:pointer im-image)))

(cffi:defcfun (%im-process-norm-diff-ratio "imProcessNormDiffRatio") counter-aborted
  (src-image1 im-image)
  (src-image2 im-image)
  (dst-image im-image))

;;; im_process additional render synthesis

(cffi:defcfun (%im-process-calc-rotate-size-extra "imProcessCalcRotateSize") :void
  (width :int)
  (height :int)
  (new-width-ptr (:pointer :int))
  (new-height-ptr (:pointer :int))
  (cos0 :double)
  (sin0 :double))
