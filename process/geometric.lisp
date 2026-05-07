(defpackage #:im-geometric
  (:use #:common-lisp)
  (:export #:resize
           #:reduce-image
           #:reduce-by-4
           #:rotate
           #:rotate-90
           #:rotate-180
           #:rotate-ref
           #:radial
           #:swirl
           #:lens-distort
           #:mirror
           #:flip
           #:crop
           #:add-margins
           #:insert
           #:interlace-split
           #:calc-rotate-size))

(in-package #:im-geometric)

(defun resize (src-image dst-image &optional (order 1))
  "Resample SRC-IMAGE into DST-IMAGE. ORDER is the interpolation order:
0 (nearest), 1 (bilinear), 3 (bicubic). Signals COUNTER-ABORTED."
  (im-process-cffi::%im-process-resize src-image dst-image order))

(defun reduce-image (src-image dst-image &optional (order 1))
  "Reduce (downsample) SRC-IMAGE into DST-IMAGE using interpolation
ORDER. Signals COUNTER-ABORTED."
  (im-process-cffi::%im-process-reduce src-image dst-image order))

(defun reduce-by-4 (src-image dst-image)
  "Reduce by 4 (each output pixel = mean of a 2x2 source block, so
each dimension is halved). DST-IMAGE must be sized W/2 x H/2.
Signals COUNTER-ABORTED."
  (im-process-cffi::%im-process-reduce-by-4 src-image dst-image))

(defun rotate (src-image dst-image cos0 sin0 &optional (order 1))
  "Rotate SRC-IMAGE about its centre using cos and sin of the angle.
Signals COUNTER-ABORTED."
  (im-process-cffi::%im-process-rotate src-image dst-image
                                       (coerce cos0 'double-float)
                                       (coerce sin0 'double-float)
                                       order))

(defun rotate-90 (src-image dst-image &key (clockwise t))
  "Rotate SRC-IMAGE 90 degrees. CLOCKWISE selects direction.
Signals COUNTER-ABORTED."
  (im-process-cffi::%im-process-rotate-90 src-image dst-image (if clockwise 1 0)))

(defun rotate-180 (src-image dst-image)
  "Rotate SRC-IMAGE 180 degrees. Signals COUNTER-ABORTED."
  (im-process-cffi::%im-process-rotate-180 src-image dst-image))

(defun rotate-ref (src-image dst-image cos0 sin0 ref-x ref-y to-origin order)
  "Rotate about (REF-X, REF-Y). If TO-ORIGIN is true, the reference
point is moved to the destination origin. Signals COUNTER-ABORTED."
  (im-process-cffi::%im-process-rotate-ref src-image dst-image
                                           (coerce cos0 'double-float)
                                           (coerce sin0 'double-float)
                                           ref-x ref-y to-origin order))

(defun radial (src-image dst-image k1 &optional (order 1))
  "Apply radial distortion. K1 is the distortion coefficient.
Signals COUNTER-ABORTED."
  (im-process-cffi::%im-process-radial src-image dst-image
                                       (coerce k1 'double-float) order))

(defun swirl (src-image dst-image k1 &optional (order 1))
  "Apply swirl distortion. Signals COUNTER-ABORTED."
  (im-process-cffi::%im-process-swirl src-image dst-image
                                      (coerce k1 'double-float) order))

(defun lens-distort (src-image dst-image a b c &optional (order 1))
  "Apply lens distortion correction (a, b, c are the polynomial
coefficients, see imProcessLensDistort). Signals COUNTER-ABORTED."
  (im-process-cffi::%im-process-lens-distort src-image dst-image
                                             (coerce a 'double-float)
                                             (coerce b 'double-float)
                                             (coerce c 'double-float)
                                             order))

(defun mirror (src-image &optional (dst-image src-image))
  "Horizontal flip; in-place by default. Signals COUNTER-ABORTED."
  (im-process-cffi::%im-process-mirror src-image dst-image))

(defun flip (src-image &optional (dst-image src-image))
  "Vertical flip; in-place by default. Signals COUNTER-ABORTED."
  (im-process-cffi::%im-process-flip src-image dst-image))

(defun crop (src-image dst-image xmin ymin)
  "Copy a sub-rectangle of SRC-IMAGE starting at (XMIN, YMIN) into
DST-IMAGE. The destination size determines the crop window.
Signals COUNTER-ABORTED."
  (im-process-cffi::%im-process-crop src-image dst-image xmin ymin))

(defun add-margins (src-image dst-image xmin ymin)
  "Place SRC-IMAGE inside DST-IMAGE at offset (XMIN, YMIN); pixels
outside SRC are zeroed. Signals COUNTER-ABORTED."
  (im-process-cffi::%im-process-add-margins src-image dst-image xmin ymin))

(defun insert (src-image region-image dst-image xmin ymin)
  "Insert REGION-IMAGE into a copy of SRC-IMAGE at (XMIN, YMIN).
Signals COUNTER-ABORTED."
  (im-process-cffi::%im-process-insert src-image region-image dst-image xmin ymin))

(defun interlace-split (src-image dst1 dst2)
  "Split alternate scan-lines of SRC-IMAGE into DST1 (even rows) and
DST2 (odd rows). Signals COUNTER-ABORTED."
  (im-process-cffi::%im-process-interlace-split src-image dst1 dst2))

(defun calc-rotate-size (width height cos0 sin0)
  "Calculate the destination dimensions needed to fit a rotated
image; returns (values NEW-WIDTH NEW-HEIGHT)."
  (cffi:with-foreign-objects ((nw :int) (nh :int))
    (im-process-cffi::%im-process-calc-rotate-size
     width height nw nh
     (coerce cos0 'double-float)
     (coerce sin0 'double-float))
    (values (cffi:mem-ref nw :int) (cffi:mem-ref nh :int))))
