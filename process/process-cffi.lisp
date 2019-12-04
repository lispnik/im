(defpackage #:im-process-cffi
  (:use #:common-lisp
        #:im-cffi)
  (:export #:counter-aborted)
  (:import-from #:tecgraf-base
                #:im-image))

(in-package #:im-process-cffi)

#+im-process-use-openmp
(cffi:define-foreign-library lib-im-process
  (:unix "libim_process_omp.so")
  (:windows "im_process_omp.dll")
  (t (:default "im_process_omp")))

#-im-process-use-openmp
(cffi:define-foreign-library lib-im-process
  (:unix "libim_process.so")
  (:windows "im_process.dll")
  (t (:default "im_process")))

(cffi:use-foreign-library lib-im-process)

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

(cffi:defcfun (%im-process-render-add-gaussian-noise "imProcessRenderAddGuassianNoise") counter-aborted
  (src-im-image im-image)
  (dst-im-image im-image)
  (mean :double)
  (std-dev :double))

(cffi:defcfun (%im-process-render-add-uniform-noise "imProcessRenderAddUnifromNoise") counter-aborted
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

(cffi:defcfun (%im-process-render-gaussian "imProcessRenderGuassian") counter-aborted
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
