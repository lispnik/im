(defpackage #:im-calc
  (:use #:common-lisp)
  (:export #:rms-error
	   #:snr
	   #:count-colors
	   #:stats-max
	   #:stats-min
	   #:stats-positive
	   #:stats-negative
	   #:stats-zeros
	   #:stats-mean
	   #:stats-stddev
	   #:image-statistics
	   #:stats-extra-median
	   #:stats-extra-mode
	   #:image-histogram-statistics
	   #:percent-min-max))

(in-package #:im-calc)

(defun rms-error (im-image-1 im-image-2)
  "Calculates the RMS error between two images (Root Mean Square
Error). 

Signals COUNTER-ABORTED if the counter aborted."
  (cffi:with-foreign-object (rms-error-ptr :double)
    (if (im-process-cffi::%im-calc-rms-error
	 im-image-1 im-image-2
	 rms-error-ptr)
	(cffi:mem-ref rms-error-ptr :double)
	(signal 'counter-aborted))))

(defun snr (src-im-image noise-im-image)
  "Calculates the SNR of an image and its noise (Signal Noise
Ratio). 

Signals COUNTER-ABORTED if the counter aborted."
  (cffi:with-foreign-object (snr-ptr :double)
    (if (im-process-cffi::%im-calc-snr
	 src-im-image noise-im-image
	 snr-ptr)
	(cffi:mem-ref snr-ptr :double)
	(signal 'counter-aborted))))

(defun count-colors (im-image)
  "Count the number of different colors in an image. 

Image data-type must be :DATA-TYPE-BYTE, but can have all color spaces
except :COLOR-SPACE-CMYK. Data type can be also :DATA-TYPE-SHORT or
:DATA-TYPE-USHORT if the color space is :COLOR-SPACE-GRAY,
:COLOR-SPACE-BINARY or :COLOR-SPACE-MAP.

Does not use OpenMP when enabled, when color space depth is greater
than 1.

Signals COUNTER-ABORTED if the counter aborted."
  (let ((data-type (im-image:data-type im-image))
	(color-space (im-image:color-space im-image)))
    (assert
     (or (and (eq data-type :data-type-byte)
	      (not (eq color-space :color-space-cmyk)))
	 (and (member data-type
		      '(:data-type-short :data-type-ushort))
	      (member color-space
		      '(:color-space-gray :color-space-binary :color-space-map))))))
  (cffi:with-foreign-object (count-ptr :unsigned-long)
    (if (im-process-cffi::%im-calc-count-colors im-image count-ptr)
	(cffi:mem-ref count-ptr :unsigned-long)
	(signal 'counter-aborted))))

;;; TODO...

;; imHistogramNew
;; imHistogramRelease
;; imHistogramShift
;; imHisogramCount

(cffi:define-c-struct-wrapper (stats im-process-cffi::im-stats-struct) ())

(defun image-statistics (im-image)
  "Calculates the statistics about the image data. There is one STATS
for each depth plane (e.g. red, green, blue stats) the result is a
sequence of STATS. 

Supports all data types except complex.

Signals COUNTER-ABORTED if the counter aborted."
  (assert (not (member (im-image:data-type im-image) '(:data-type-cfloat :data-type-cdouble))))
  (let ((depth (im-image:depth im-image)))
    (cffi:with-foreign-object
	(stats-ptr '(:struct im-process-cffi::im-stats-struct) depth)
      (if (im-process-cffi::%im-calc-image-statistics im-image stats-ptr)
	  (loop with result = (make-array depth :element-type 'stats)
		and size = (cffi:foreign-type-size 'im-process-cffi::im-stats-struct)
		for i below depth
		for stats-i-ptr = (cffi:inc-pointer
				   stats-ptr
				   (* i size))
		do (setf (aref result i)
			 (make-instance 'stats :pointer stats-i-ptr))
		finally (return result))
	  (signal 'counter-aborted)))))


(defstruct stats-extra median mode)

(defun histogram-image-statistics (im-image)
  "Calculates some extra statistics about the image histogram data.
Returns an array of STATS-EXTRA. There is one STATS-EXTRA for each
depth plane.

Only :DATA-TYPE-BYTE, :DATA-TYPE-SHORT and :DATA-TYPE-USHORT images
are supported.  Mode will be NIL if more than one max is
found. 

Signals COUNTER-ABORTED if the counter aborted."
  (assert (member (im-image:data-type im-image) '(:data-type-byte :data-type-short :data-type-ushort)))
  ;; FIXME more than one plane
  (let ((depth (im-image:depth im-image)))
    (cffi:with-foreign-objects
	((median-ptr :int depth)
	 (mode-ptr :int depth))
      (if (im-process-cffi::%im-calc-histo-image-statistics
	   im-image median-ptr mode-ptr)
	  (loop with result = (make-array depth :element-type 'stats-extra)
		for i below depth
		for mode = (cffi:mem-aref mode-ptr :int i)
		do (setf (aref result i)
			 (make-stats-extra :median (cffi:mem-aref median-ptr :int i)
					   :mode (if (= -1 mode) nil mode)))
		finally (return result))
	  (signal 'counter-aborted)))))

(defun percent-min-max (im-image percent ignore-zero-p)
  "Calculates the minimum and maximum levels ignoring a given
percentage of the histogram count.

Used by EXPAND-HISTOGRAM.  Only :DATA-TYPE-BYTE, :DATA-TYPE-SHORT and
:DATA-TYPE-USHORT images are supported.

Signals COUNTER-ABORTED if the counter aborted."
  (assert (member (im-image:data-type im-image) '(:data-type-byte :data-type-short :data-type-ushort)))
  (cffi:with-foreign-objects
      ((min-ptr :int)
       (max-ptr :int))
    (if (im-process-cffi::%im-calc-percent-min-max
	 im-image (coerce percent 'double-float) ignore-zero-p min-ptr max-ptr)
	(values (cffi:mem-ref min-ptr :int)
		(cffi:mem-ref max-ptr :int))
	(signal 'counter-aborted))))
