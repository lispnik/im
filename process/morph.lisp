(defpackage #:im-morph
  (:use #:common-lisp)
  (:export #:gray-erode
           #:gray-dilate
           #:gray-open
           #:gray-close
           #:gray-top-hat
           #:gray-well
           #:gray-gradient
           #:gray-convolve
           #:bin-erode
           #:bin-dilate
           #:bin-open
           #:bin-close
           #:bin-outline
           #:bin-thin
           #:bin-thin-zhang-suen
           #:bin-thin-nh-maps
           #:bin-convolve))

(in-package #:im-morph)

;;; Gray morphology --------------------------------------------------------

(defun gray-erode (src-image dst-image kernel-size)
  "Apply gray-scale erosion (per-window minimum) using a square kernel
of side KERNEL-SIZE. Signals COUNTER-ABORTED if the counter aborted."
  (im-process-cffi::%im-process-gray-morph-erode src-image dst-image kernel-size))

(defun gray-dilate (src-image dst-image kernel-size)
  "Apply gray-scale dilation (per-window maximum) using a square kernel
of side KERNEL-SIZE. Signals COUNTER-ABORTED if the counter aborted."
  (im-process-cffi::%im-process-gray-morph-dilate src-image dst-image kernel-size))

(defun gray-open (src-image dst-image kernel-size)
  "Erode then dilate with a square kernel of side KERNEL-SIZE.
Signals COUNTER-ABORTED if the counter aborted."
  (im-process-cffi::%im-process-gray-morph-open src-image dst-image kernel-size))

(defun gray-close (src-image dst-image kernel-size)
  "Dilate then erode with a square kernel of side KERNEL-SIZE.
Signals COUNTER-ABORTED if the counter aborted."
  (im-process-cffi::%im-process-gray-morph-close src-image dst-image kernel-size))

(defun gray-top-hat (src-image dst-image kernel-size)
  "Top-hat = src - open(src). Highlights bright structures smaller
than the kernel. Signals COUNTER-ABORTED if the counter aborted."
  (im-process-cffi::%im-process-gray-morph-top-hat src-image dst-image kernel-size))

(defun gray-well (src-image dst-image kernel-size)
  "Well = close(src) - src. Highlights dark structures smaller than
the kernel. Signals COUNTER-ABORTED if the counter aborted."
  (im-process-cffi::%im-process-gray-morph-well src-image dst-image kernel-size))

(defun gray-gradient (src-image dst-image kernel-size)
  "Morphological gradient = dilate(src) - erode(src). Approximates
edge magnitude. Signals COUNTER-ABORTED if the counter aborted."
  (im-process-cffi::%im-process-gray-morph-gradient src-image dst-image kernel-size))

(defun gray-convolve (src-image dst-image kernel ismax)
  "Apply a structuring-element convolution. ISMAX selects max (T) or
min (NIL). Signals COUNTER-ABORTED if the counter aborted."
  (im-process-cffi::%im-process-gray-morph-convolve src-image dst-image kernel ismax))

;;; Binary morphology ------------------------------------------------------

(defun bin-erode (src-image dst-image kernel-size &optional (iterations 1))
  "Binary erosion using a square kernel of side KERNEL-SIZE, repeated
ITERATIONS times. Signals COUNTER-ABORTED if the counter aborted."
  (im-process-cffi::%im-process-bin-morph-erode src-image dst-image kernel-size iterations))

(defun bin-dilate (src-image dst-image kernel-size &optional (iterations 1))
  "Binary dilation using a square kernel of side KERNEL-SIZE, repeated
ITERATIONS times. Signals COUNTER-ABORTED if the counter aborted."
  (im-process-cffi::%im-process-bin-morph-dilate src-image dst-image kernel-size iterations))

(defun bin-open (src-image dst-image kernel-size &optional (iterations 1))
  "Binary open = erode then dilate. Signals COUNTER-ABORTED if the
counter aborted."
  (im-process-cffi::%im-process-bin-morph-open src-image dst-image kernel-size iterations))

(defun bin-close (src-image dst-image kernel-size &optional (iterations 1))
  "Binary close = dilate then erode. Signals COUNTER-ABORTED if the
counter aborted."
  (im-process-cffi::%im-process-bin-morph-close src-image dst-image kernel-size iterations))

(defun bin-outline (src-image dst-image kernel-size &optional (iterations 1))
  "Extract the foreground outline (foreground minus its erosion).
Signals COUNTER-ABORTED if the counter aborted."
  (im-process-cffi::%im-process-bin-morph-outline src-image dst-image kernel-size iterations))

(defun bin-thin (src-image dst-image)
  "Skeletonize a binary image. Signals COUNTER-ABORTED if the counter
aborted."
  (im-process-cffi::%im-process-bin-morph-thin src-image dst-image))

(defun bin-thin-zhang-suen (src-image dst-image)
  "Zhang-Suen thinning. Signals COUNTER-ABORTED if the counter aborted."
  (im-process-cffi::%im-process-bin-thin-zhang-suen src-image dst-image))

(defun bin-thin-nh-maps (src-image dst-image)
  "Neighbourhood-map thinning. Signals COUNTER-ABORTED if the counter
aborted."
  (im-process-cffi::%im-process-bin-thin-nh-maps src-image dst-image))

(defun bin-convolve (src-image dst-image kernel hit-white iter)
  "Hit-or-miss style binary convolution. Signals COUNTER-ABORTED if
the counter aborted."
  (im-process-cffi::%im-process-bin-morph-convolve src-image dst-image kernel hit-white iter))
