(defpackage #:im-process-cffi
  (:use #:common-lisp
	#:im-cffi))

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

;;; im_process_ana.h

(cffi:defcfun (%im-calc-rms-error "imCalcRMSError") :boolean
  (im-image-1 im-image)
  (im-image-2 im-image)
  (rms-error-ptr (:pointer :double)))

(cffi:defcfun (%im-calc-snr "imCalcSNR") :boolean
  (src-im-image im-image)
  (noise-im-image im-image)
  (snr-ptr (:pointer :double)))

(cffi:defcfun (%im-calc-count-colors "imCalcCountColors") :boolean
  (im-image im-image)
  (count-ptr (:pointer :double)))

(cffi:defcfun (%im-calc-gray-histogram "imCalcGrayHistogram") :boolean
  (im-image im-image)
  (histo-ptr (:pointer :long))
  (cumulative :boolean))

(cffi:defcfun (%im-calc-histogram "imCalcHistogram") :boolean
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
  (data-type data-type)
  (hcount-ptr (:pointer :int)))

(cffi:defcfun (%im-histogram-release "imHistogramRelease") :void
  (histo-ptr (:pointer :unsigned-long)))

(cffi:defcfun (%im-histogram-shift "imHistogramShift") :int
  (data-type data-type))

(cffi:defcfun (%im-histogram-count "imHistogramCount") :int
  (data-type data-type))

(cffi:defcstruct im-stats-struct
  (max :double)
  (min :double)
  (positive :unsigned-long)
  (negative :unsigned-long)
  (zeros :unsigned-long)
  (mean :double)
  (stddev :double))

(cffi:defctype im-stats (:pointer (:struct im-stats-struct)))

(cffi:defcfun (%im-calc-image-statistics "imCalcImageStatistics") :boolean
  (image im-image)
  (stats im-stats))

(cffi:defcfun (%im-calc-histogram-statistics "imCalcHistogramStatistics") :boolean
  (image im-image)
  (stats im-stats))

(cffi:defcfun (%im-calc-histo-image-statistics "imCalcHistoImageStatistics") :boolean
  (image im-image)
  (median (:pointer :int))
  (mode (:pointer :int)))

(cffi:defcfun (%im-calc-percent-min-max "imCalcPercentMinMax") :boolean
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

(cffi:defcfun (%im-process-rotate "imProcessRotate") :int
  (src-image im-image)
  (dst-image im-image)
  (cos0 :double)
  (sin0 :double)
  (order :int))

(cffi:defcfun (%im-process-rotate-ref "imProcessRotateRef") :int
  (src-image im-image)
  (dst-image im-image)
  (cos0 :double)
  (sin0 :double)
  (x :int)
  (y :int)
  (to-origin :boolean)
  (order :int))

(cffi:defcfun (%im-process-rotate-90 "imProcessRotate90") :boolean
  (src-image im-image)
  (dst-image im-image)
  (dir-clockwise :int))

(cffi:defcfun (%im-process-rotate-180 "imProcessRotate180") :boolean
  (src-image im-image)
  (dst-image im-image))

(cffi:defcfun (%im-process-mirror "imProcessMirror") :boolean
  (src-image im-image)
  (dst-image im-image))

(cffi:defcfun (%im-process-flip "imProcessFlip") :boolean
  (src-image im-image)
  (dst-image im-image))

(cffi:defcfun (%im-process-radial "imProcessRadial") :boolean
  (src-image im-image)
  (dst-image im-image)
  (k1 :double)
  (order :int))

(cffi:defcfun (%im-process-lens-distort "imProcessLensDistort") :boolean
  (src-image im-image)
  (dst-image im-image)
  (a :double)
  (b :double)
  (c :double)
  (order :int))

(cffi:defcfun (%im-process-swirl "imProcessSwirl") :boolean
  (src-image im-image)
  (dst-image im-image)
  (k1 :double)
  (order :int))

(cffi:defcfun (%im-process-interlace-split "imProcessInterlaceSplit") :boolean
  (src-image im-image)
  (dst-image1 im-image)
  (dst-image2 im-image))
