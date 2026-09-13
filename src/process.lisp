;;;; src/process.lisp — the im_process operations.
;;;;
;;;; Almost every function here has the same shape: take a source image and a
;;;; destination image, return an int that is zero when the progress counter
;;;; cancelled the work. DEFINE-PROCESS-OP writes that shape once, so each
;;;; operation below is its signature, its docstring and nothing else -- and so
;;;; every one of them gets the cancellation restarts rather than the handful
;;;; someone remembered to wrap.

(in-package #:im)

(export '(resize
          crop
          rotate-90
          rotate-180
          mirror
          flip
          negative
          convolve-sobel
          convolve-prewitt
          convolve-gaussian
          convolve-median
          canny
          unsharp
          denoise-bilateral
          denoise-anisotropic-diffusion
          denoise-non-local-means
          *diffusion-functions*
          render-gaussian
          deconvolve-richardson-lucy
          threshold
          threshold-otsu
          morph-erode
          morph-dilate
          morph-open
          morph-close
          fft
          ifft
          swap-quadrants
          complex-image-p
          convert-data-type
          convert-color-space
          convert-to-bitmap
          histogram
          statistics
          rms-error
          signal-to-noise-ratio
          count-colors
          find-regions
          make-label-image
          watershed
          watershed-segment
          region-areas
          region-centroids
          region-bounding-boxes
          region-convex-hulls
          region-feret-diameters
          region-intensities
          decorrelation-stretch
          decorrelation-fit
          decorrelation-apply
          decorrelation-transform
          decorrelation-transform-p
          decorrelation-matrix
          decorrelation-offset
          decorrelation-mean
          decorrelation-target
          decorrelation-stddev
          decorrelation-rank
          decorrelation-space))

(defmacro define-process-op (name lambda-list c-function &body body)
  "Define an im_process wrapper that participates in cancellation.

BODY is the docstring followed by the argument forms passed to C-FUNCTION.
The call is wrapped in CALL-WITH-CANCELLATION-RESTARTS, so an operation
stopped by a progress callback signals OPERATION-ABORTED and offers RETRY and
CONTINUE instead of returning NIL and letting the caller read a partly written
destination."
  (let ((doc (when (stringp (first body)) (first body)))
        (args (if (stringp (first body)) (rest body) body)))
    `(defun ,name ,lambda-list
       ,@(when doc (list doc))
       (check-operation ,(string-downcase (symbol-name name))
         (not (zerop (,c-function ,@args)))))))

;;; Geometry ------------------------------------------------------------------

(define-process-op resize (src dst &optional (order 1)) im.ffi::%im-process-resize
  "Resample SRC into DST, which sets the output size.

ORDER is the interpolation order: 0 nearest neighbour, 1 bilinear, 3 bicubic."
  (handle src) (handle dst) order)

(define-process-op crop (src dst x y) im.ffi::%im-process-crop
  "Copy the DST-sized rectangle at (X, Y) out of SRC.

Remember that IM images are bottom-up: Y counts from the bottom edge."
  (handle src) (handle dst) x y)

(define-process-op rotate-90 (src dst direction) im.ffi::%im-process-rotate90
  "Rotate by 90 degrees. DIRECTION is 1 for clockwise, -1 for anticlockwise.

DST must have SRC's width and height exchanged."
  (handle src) (handle dst) direction)

(define-process-op rotate-180 (src dst) im.ffi::%im-process-rotate180
  "Rotate by 180 degrees into a DST of the same size."
  (handle src) (handle dst))

(define-process-op mirror (src dst) im.ffi::%im-process-mirror
  "Reflect left to right."
  (handle src) (handle dst))

(define-process-op flip (src dst) im.ffi::%im-process-flip
  "Reflect top to bottom."
  (handle src) (handle dst))

;;; Point operations ----------------------------------------------------------

(defun negative (src dst)
  "Invert SRC into DST, in the colour space's own terms. Returns DST.

Not wrapped in CHECK-OPERATION: imProcessNegative returns void, so there is no
cancellation flag to test, and treating its non-existent return value as one
raised a type error on NIL."
  (im.ffi::%im-process-negative (handle src) (handle dst))
  dst)

;;; Convolution and edges -----------------------------------------------------

(define-process-op convolve-sobel (src dst) im.ffi::%im-process-sobel-convolve
  "Sobel edge magnitude."
  (handle src) (handle dst))

(define-process-op convolve-prewitt (src dst) im.ffi::%im-process-prewitt-convolve
  "Prewitt edge magnitude."
  (handle src) (handle dst))

(define-process-op convolve-gaussian (src dst stddev)
    im.ffi::%im-process-gaussian-convolve
  "Gaussian blur with the given standard deviation."
  (handle src) (handle dst) (coerce stddev 'double-float))

(define-process-op convolve-median (src dst size) im.ffi::%im-process-median-convolve
  "Median filter over a SIZE by SIZE neighbourhood."
  (handle src) (handle dst) size)

(define-process-op canny (src dst stddev) im.ffi::%im-process-canny
  "Canny edge detection. STDDEV sets the Gaussian smoothing.

Thresholds are estimated by IM; DST must be a one-plane image."
  (handle src) (handle dst) (coerce stddev 'double-float))

(define-process-op unsharp (src dst stddev amount threshold)
    im.ffi::%im-process-unsharp
  "Unsharp mask: subtract a Gaussian blur to sharpen."
  (handle src) (handle dst)
  (coerce stddev 'double-float)
  (coerce amount 'double-float)
  (coerce threshold 'double-float))

;;; Edge-preserving denoising and deconvolution ------------------------------
;;;
;;; The four functions here check their arguments before calling, which the
;;; rest of this file leaves to IM. They have to: each of these C entry points
;;; validates its preconditions and reports a violation by returning
;;; IM_PROCESS_ABORT -- the same zero the progress counter returns when the
;;; user cancels. Passed straight through, an even-sided PSF or a time step of
;;; 0.5 would arrive as OPERATION-ABORTED saying the operation was cancelled,
;;; which is the wrong diagnosis for a wrong argument.

(defun %check-same-type-size (src dst what)
  (unless (and (= (width src) (width dst))
               (= (height src) (height dst))
               (eq (data-type src) (data-type dst))
               (= (depth src) (depth dst)))
    (cl:error 'data-error
              :detail (format nil "~A needs images of the same size and type, got ~Dx~D ~(~A~) and ~Dx~D ~(~A~)"
                              what
                              (width src) (height src) (data-type src)
                              (width dst) (height dst) (data-type dst))))
  (when (complex-image-p src)
    (cl:error 'data-error
              :detail (format nil "~A does not support complex samples" what))))

(defun denoise-bilateral (src dst spatial-stddev range-stddev)
  "Bilateral filter: Gaussian smoothing that does not average across an edge.

SPATIAL-STDDEV is in pixels and sets the neighbourhood. RANGE-STDDEV is in the
image's own sample units and is the whole of the difference from a plain
Gaussian: a neighbour further than about that from the centre pixel's value
barely contributes, so a step survives. Set it near the noise level -- much
above it and texture flattens into patches. Cost grows with the square of
SPATIAL-STDDEV.

SRC and DST may be the same image."
  (%check-same-type-size src dst "denoise-bilateral")
  (unless (and (plusp spatial-stddev) (plusp range-stddev))
    (cl:error 'data-error
              :detail (format nil "bilateral needs positive standard deviations, got ~A and ~A"
                              spatial-stddev range-stddev)))
  (check-operation "denoise-bilateral"
    (not (zerop (im.ffi::%im-process-bilateral-filter
                 (handle src) (handle dst)
                 (coerce spatial-stddev 'double-float)
                 (coerce range-stddev 'double-float))))))

(defparameter *diffusion-functions*
  '(:exponential :quadratic :tukey)
  "The conductance functions IM:DENOISE-ANISOTROPIC-DIFFUSION accepts.

Short names rather than the enum's :DIFFUSION-FUNC-EXPONENTIAL spelling. The
fully-prefixed form is right for a data type or a colour space, which appear
in output and read as their own nouns; this one is a private choice of one of
three formulas and never leaves the call. Same trade as *COMPLEX-PARTS*.")

(defparameter %diffusion-function-keywords
  '((:exponential . :diffusion-func-exponential)
    (:quadratic   . :diffusion-func-quadratic)
    (:tukey       . :diffusion-func-tukey))
  "Short name to generated enum keyword.

The value handed to C comes from the enum rather than from a position in
*DIFFUSION-FUNCTIONS*, so a reordering upstream is picked up by regenerating
src/ffi/ instead of silently selecting the wrong formula.")

(defun %diffusion-function-value (function)
  (let ((keyword (cdr (assoc function %diffusion-function-keywords))))
    (unless keyword
      (cl:error 'im-error
                :detail (format nil "~S is not a diffusion function; expected one of ~S"
                                function *diffusion-functions*)))
    (cffi:foreign-enum-value 'im.ffi::diffusion-func keyword)))

(defun denoise-anisotropic-diffusion (src dst &key (time-step 0.2d0) (kappa 30.0d0)
                                                   (iterations 10)
                                                   (function :exponential))
  "Perona-Malik anisotropic diffusion: smooth within regions, not across edges.

KAPPA is the gradient threshold in the image's own sample units -- a step
larger than it is treated as an edge and stops conducting, so edges survive
and sharpen while flat regions smooth. FUNCTION picks the conductance formula:
:EXPONENTIAL favours high-contrast edges, :QUADRATIC favours wide regions, and
:TUKEY has compact support, so an edge above KAPPA stops moving entirely.

TIME-STEP must be in (0, 0.25]. The scheme is explicit and unstable above a
quarter with four neighbours, where it diverges into a checkerboard rather
than reporting anything -- which is why the bound is checked here and not left
to the caller to discover.

SRC and DST may be the same image."
  (%check-same-type-size src dst "denoise-anisotropic-diffusion")
  (unless (and (plusp time-step) (<= time-step 0.25d0))
    (cl:error 'data-error
              :detail (format nil "diffusion time step must be in (0, 0.25], got ~A"
                              time-step)))
  (unless (plusp kappa)
    (cl:error 'data-error
              :detail (format nil "diffusion kappa must be positive, got ~A" kappa)))
  (unless (and (integerp iterations) (not (minusp iterations)))
    (cl:error 'data-error
              :detail (format nil "diffusion iterations must be a non-negative integer, got ~S"
                              iterations)))
  (check-operation "denoise-anisotropic-diffusion"
    (not (zerop (im.ffi::%im-process-anisotropic-diffusion
                 (handle src) (handle dst)
                 (coerce time-step 'double-float)
                 (coerce kappa 'double-float)
                 iterations
                 (%diffusion-function-value function))))))

(defun denoise-non-local-means (src dst &key (search-radius 5) (patch-radius 2)
                                             (filter-stddev 10.0d0))
  "Non-local means: average pixels whose NEIGHBOURHOODS match, not their neighbours.

Repeated fine structure -- brickwork, fabric, text -- is recovered rather than
smoothed away, because the pixels that vote for a given pixel are the ones
sitting in a similar patch, wherever in the search window they are.
FILTER-STDDEV is the weight decay in the image's own sample units; start at the
noise standard deviation.

Cost grows with SEARCH-RADIUS squared times PATCH-RADIUS squared, which makes
this one to two orders of magnitude slower than the other two filters here.
The defaults are the usual starting point.

SRC and DST may be the same image."
  (%check-same-type-size src dst "denoise-non-local-means")
  (unless (and (plusp search-radius) (plusp patch-radius) (plusp filter-stddev))
    (cl:error 'data-error
              :detail (format nil "non-local means needs positive radii and stddev, got ~A, ~A and ~A"
                              search-radius patch-radius filter-stddev)))
  (check-operation "denoise-non-local-means"
    (not (zerop (im.ffi::%im-process-non-local-means
                 (handle src) (handle dst)
                 search-radius patch-radius
                 (coerce filter-stddev 'double-float))))))

(defun render-gaussian (image stddev)
  "Fill IMAGE with a centred Gaussian of the given standard deviation. Returns IMAGE.

Here because it is how a point spread function is built: IM normalizes the PSF
to sum 1 internally, so a rendered Gaussian of odd dimensions is a usable
argument to DECONVOLVE-RICHARDSON-LUCY without any scaling."
  (check-operation "render-gaussian"
    (not (zerop (im.ffi::%im-process-render-gaussian
                 (handle image) (coerce stddev 'double-float)))))
  image)

(defun deconvolve-richardson-lucy (src psf dst &key (iterations 20))
  "Richardson-Lucy deconvolution of SRC by the point spread function PSF.

The maximum-likelihood restoration under Poisson noise, which is what a
photon-counting detector has. PSF must be a one-plane image with BOTH
dimensions odd -- an even-sided kernel has no centre pixel, so the result
would come out shifted half a pixel with nothing to say so -- and is
normalized to sum 1 internally.

ITERATIONS is the only regularisation there is. The iteration does not
converge to something pleasant: past a few tens of steps it fits the noise,
which shows as ringing around bright features that grows with every further
one. 10 to 50 is the usual range.

SRC and DST may be the same image."
  (%check-same-type-size src dst "deconvolve-richardson-lucy")
  (unless (= 1 (depth psf))
    (cl:error 'data-error
              :detail (format nil "the point spread function must have one plane, got ~D"
                              (depth psf))))
  (unless (and (oddp (width psf)) (oddp (height psf)))
    (cl:error 'data-error
              :detail (format nil "the point spread function must have odd dimensions, got ~Dx~D"
                              (width psf) (height psf))))
  (unless (and (integerp iterations) (not (minusp iterations)))
    (cl:error 'data-error
              :detail (format nil "deconvolution iterations must be a non-negative integer, got ~S"
                              iterations)))
  (check-operation "deconvolve-richardson-lucy"
    (not (zerop (im.ffi::%im-process-richardson-lucy
                 (handle src) (handle psf) (handle dst) iterations)))))

;;; Thresholding --------------------------------------------------------------

(define-process-op threshold (src dst level &optional (value 1))
    im.ffi::%im-process-threshold
  "Binarise at LEVEL: samples above it become VALUE, the rest zero."
  (handle src) (handle dst) (coerce level 'double-float) value)

(defun threshold-otsu (src dst)
  "Binarise SRC into DST at the level Otsu's method chooses. Returns the level.

SRC must be gray (byte, short or ushort) and DST binary.

Returning the level is the point of using this over a fixed THRESHOLD: IM
picks the value that best separates the histogram's two modes, and which
value that turned out to be is usually as interesting as the image.

Deliberately NOT wrapped in CHECK-OPERATION. Every other function here returns
an int that is zero when the counter cancelled it; this one returns the
threshold, where zero is a perfectly good answer for a very dark image.
Treating it as a cancellation flag would raise OPERATION-ABORTED on a
legitimate result -- and calling the C function a second time to get the level
back, as an earlier draft of this did, runs the whole computation twice and
writes DST twice."
  (im.ffi::%im-process-otsu-threshold (handle src) (handle dst)))

;;; Morphology ----------------------------------------------------------------
;;;
;;; IM keeps binary and greyscale morphology apart, because a binary image is
;;; stored one sample per pixel with values 0 and 1 and the greyscale
;;; operators would treat those as intensities. Dispatching on the image's own
;;; colour space removes a choice the caller should not have to make.

(macrolet ((define-morphology (name binary-fn gray-fn doc)
             `(defun ,name (src dst &key (size 3) (iterations 1))
                ,doc
                (check-operation ,(string-downcase (symbol-name name))
                  (not (zerop
                        (if (eq :color-space-binary (color-space src))
                            ;; The binary operators take an iteration count;
                            ;; the greyscale ones have no such parameter, so
                            ;; passing one is an arity error rather than a
                            ;; harmless extra.
                            (,binary-fn (handle src) (handle dst) size iterations)
                            (,gray-fn (handle src) (handle dst) size))))))))
  (define-morphology morph-erode
    im.ffi::%im-process-bin-morph-erode im.ffi::%im-process-gray-morph-erode
    "Erode with a SIZE by SIZE structuring element.")
  (define-morphology morph-dilate
    im.ffi::%im-process-bin-morph-dilate im.ffi::%im-process-gray-morph-dilate
    "Dilate with a SIZE by SIZE structuring element.")
  (define-morphology morph-open
    im.ffi::%im-process-bin-morph-open im.ffi::%im-process-gray-morph-open
    "Erode then dilate: removes small bright features.")
  (define-morphology morph-close
    im.ffi::%im-process-bin-morph-close im.ffi::%im-process-gray-morph-close
    "Dilate then erode: fills small dark gaps."))

;;; Fourier transform ---------------------------------------------------------
;;;
;;; These six names exist in BOTH libim_process and libim_fftw3. A plain
;;; DEFCFUN resolves them by dlsym search order, which differs between a source
;;; checkout and a dumped image, so which implementation runs would be decided
;;; by load order rather than by anything written here. Resolving the pointer
;;; against a named library makes the choice explicit, and prefers FFTW3 when
;;; it is present because that is the only reason to build the add-on.

(defmacro %call-fft (c-name &rest args)
  `(cffi:foreign-funcall-pointer
    (fft-symbol ,c-name) ()
    ,@args :void))

(defun complex-image-p (image)
  "True when IMAGE holds complex samples."
  (member (data-type image) '(:data-type-cfloat :data-type-cdouble)))

(defun %check-fft-arguments (src dst &key (src-complex nil))
  "Signal rather than segfault when the transform's preconditions are unmet.

IM documents these -- \"images must be of the same size\", \"target image
must be of type complex\", and for the inverse \"both must be of type
complex\" -- and enforces none of them. Handing imProcessIFFT a byte
destination is not an error there, it is a memory fault, which arrives as a
bare SB-SYS:MEMORY-FAULT-ERROR with no indication of which argument was
wrong."
  (unless (and (= (width src) (width dst))
               (= (height src) (height dst)))
    (cl:error 'data-error
              :detail (format nil "FFT needs images of the same size, got ~Dx~D and ~Dx~D"
                              (width src) (height src) (width dst) (height dst))))
  (unless (complex-image-p dst)
    (cl:error 'data-error
              :detail (format nil "FFT destination must be complex, got ~(~A~)"
                              (data-type dst))))
  (when (and src-complex (not (complex-image-p src)))
    (cl:error 'data-error
              :detail (format nil "inverse FFT source must be complex, got ~(~A~)"
                              (data-type src)))))

(defun fft (src dst)
  "Forward Fourier transform of the real image SRC into the complex DST.

Unnormalized, with the lowest frequency at the centre. Uses FFTW3 when
libim_fftw3 loaded; see FFTW3-AVAILABLE-P."
  (%check-fft-arguments src dst)
  (%call-fft "imProcessFFT" im.ffi::im-image (handle src) im.ffi::im-image (handle dst))
  dst)

(defun ifft (src dst)
  "Inverse Fourier transform. BOTH images must be complex.

The result is normalized by width*height. To get back to a real image,
transform into a complex destination and then CONVERT-DATA-TYPE it."
  (%check-fft-arguments src dst :src-complex t)
  (%call-fft "imProcessIFFT" im.ffi::im-image (handle src) im.ffi::im-image (handle dst))
  dst)

(defparameter *complex-parts*
  '((:real      . :complex2-real-real)
    (:imaginary . :complex2-real-imag)
    (:magnitude . :complex2-real-mag)
    (:phase     . :complex2-real-phase))
  "Friendly names for imComplex2Real's members.

The generated keywords spell the C enum name, which reads acceptably for
:DATA-TYPE-BYTE and badly for :COMPLEX2-REAL-MAG. Callers get :MAGNITUDE.")

(defparameter *cast-modes*
  '((:min-max . :cast-mode-minmax)
    (:fixed   . :cast-mode-fixed)
    (:direct  . :cast-mode-direct)
    (:user    . :cast-mode-user)))

(defun convert-data-type (src dst &key (complex-part :real)
                                       (gamma 0.0d0) (absolute nil)
                                       (cast-mode :min-max))
  "Convert SRC into DST's data type. Returns DST.

COMPLEX-PART selects what to take from a complex source: :REAL, :IMAGINARY,
:MAGNITUDE or :PHASE. CAST-MODE decides how the value range is mapped --
:MIN-MAX rescales the actual range onto the target's, :DIRECT truncates, and
:FIXED uses the type's full range."
  (flet ((lookup (table key what)
           (or (cdr (assoc key table))
               (cl:error 'im-error
                         :detail (format nil "~S is not a known ~A; expected one of ~S"
                                         key what (mapcar #'car table))))))
    (maybe-error
     (cffi:foreign-enum-keyword
      'im.ffi::error-code
      (im.ffi::%im-convert-data-type
       (handle src) (handle dst)
       (cffi:foreign-enum-value
        'im.ffi::complex2-real (lookup *complex-parts* complex-part "complex part"))
       ;; The C parameter is a double; passing an integer literal is a type
       ;; error at the alien boundary rather than a coercion.
       (coerce gamma 'double-float) (if absolute 1 0)
       (cffi:foreign-enum-value
        'im.ffi::cast-mode (lookup *cast-modes* cast-mode "cast mode"))))
     "convert-data-type"))
  dst)

(defun swap-quadrants (image &optional (inverse nil))
  "Move the frequency origin between the corner and the centre.

A raw transform puts DC at the corner; swapping quadrants puts it in the
middle, which is what makes a spectrum legible."
  (%call-fft "imProcessSwapQuadrants"
             im.ffi::im-image (handle image) :int (if inverse 1 0))
  image)

(defun convert-color-space (src dst)
  "Convert SRC into DST's colour space. Returns DST.

Both images must be the same size and data type. Use CREATE-BASED to build a
destination that differs only in colour space."
  (maybe-error
   (cffi:foreign-enum-keyword
    'im.ffi::error-code
    (im.ffi::%im-convert-color-space (handle src) (handle dst)))
   "convert-color-space")
  dst)

(defun convert-to-bitmap (src dst &key (complex-part :real) (gamma 0.0d0)
                                       (absolute nil) (cast-mode :min-max))
  "Convert SRC into DST, which must be a displayable 8-bit image.

\"Bitmap\" is IM's word for something a screen can show directly: byte
samples in RGB, gray, map or binary. This is the one conversion that handles
colour space and data type together, which is what a viewer or a thumbnail
needs."
  (maybe-error
   (cffi:foreign-enum-keyword
    'im.ffi::error-code
    (im.ffi::%im-convert-to-bitmap
     (handle src) (handle dst)
     (cffi:foreign-enum-value
      'im.ffi::complex2-real
      (or (cdr (assoc complex-part *complex-parts*)) :complex2-real-real))
     (coerce gamma 'double-float) (if absolute 1 0)
     (cffi:foreign-enum-value
      'im.ffi::cast-mode
      (or (cdr (assoc cast-mode *cast-modes*)) :cast-mode-minmax))))
   "convert-to-bitmap")
  dst)

(defun histogram (image &key (plane 0) (cumulative nil))
  "The histogram of one PLANE of IMAGE, as a vector of counts.

IMAGE must be byte, short or ushort -- a histogram of floating-point samples
has no natural bin count and IM does not offer one. The vector is 256 long for
byte data and 65536 for the 16-bit types."
  (let ((levels (case (data-type image)
                  (:data-type-byte 256)
                  ((:data-type-short :data-type-ushort) 65536)
                  (t (cl:error 'data-error
                               :detail (format nil "no histogram for ~(~A~) data"
                                               (data-type image)))))))
    (cffi:with-foreign-object (counts :unsigned-long levels)
      (im.ffi::%im-calc-histogram (handle image) counts plane (if cumulative 1 0))
      (let ((result (make-array levels)))
        (dotimes (i levels result)
          (setf (aref result i) (cffi:mem-aref counts :unsigned-long i)))))))

;;; Decorrelation stretch -----------------------------------------------------

;;; The enhancement DStretch is built on: colours strung along a single axis --
;;; faded pigment against rock, which is what the technique was made for -- are
;;; pulled apart until the differences are visible.
;;;
;;; SPACE is a :DECORRELATION-SPACE-* keyword, in the same fully-prefixed form
;;; the colour-space and data-type enums use here. Which one to reach for is a
;;; matter of what colour is being looked for; :DECORRELATION-SPACE-LDS is the
;;; general-purpose one.
;;;
;;; SCALE multiplies each band's own spread, so 1 decorrelates without adding
;;; contrast and leaves a washed-out image just as washed out. That is correct
;;; and rarely what is wanted: a faint photograph takes 6 or 8 before it clips,
;;; where a full-contrast one clips at 2.

(defstruct (decorrelation-transform (:conc-name decorrelation-))
  "A fitted decorrelation stretch, as returned by DECORRELATION-FIT.

MATRIX and OFFSET are the whole of the transform -- out = MATRIX*in + OFFSET,
over the source's own components. The rest reports what the fit found: RANK
below 3 means the colours lay in a plane or on a line, so the stretch could
not decorrelate every direction and did not pretend to."
  (matrix (make-array 9 :element-type 'double-float :initial-element 0d0)
   :type (simple-array double-float (9)))
  (offset (make-array 3 :element-type 'double-float :initial-element 0d0)
   :type (simple-array double-float (3)))
  (mean (make-array 3 :element-type 'double-float :initial-element 0d0)
   :type (simple-array double-float (3)))
  (target (make-array 3 :element-type 'double-float :initial-element 0d0)
   :type (simple-array double-float (3)))
  (stddev (make-array 3 :element-type 'double-float :initial-element 0d0)
   :type (simple-array double-float (3)))
  (rank 3 :type (integer 0 3))
  (space :decorrelation-space-rgb))

(defun %decorrelation-space-value (space)
  (handler-case (cffi:foreign-enum-value 'im.ffi::decorrelation-space space)
    (cl:error ()
      (cl:error 'im-error
                :detail (format nil "unknown decorrelation space ~S" space)))))

(defun %read-decorrelation-transform (pointer)
  (flet ((doubles (slot count)
           (let ((array (make-array count :element-type 'double-float))
                 (base (cffi:foreign-slot-pointer
                        pointer '(:struct im.ffi::im-decorrelation-transform-struct) slot)))
             (dotimes (i count array)
               (setf (aref array i) (cffi:mem-aref base :double i))))))
    (make-decorrelation-transform
     :matrix (doubles 'im.ffi::matrix 9)
     :offset (doubles 'im.ffi::offset 3)
     :mean (doubles 'im.ffi::mean 3)
     :target (doubles 'im.ffi::target 3)
     :stddev (doubles 'im.ffi::stddev 3)
     :rank (cffi:foreign-slot-value
            pointer '(:struct im.ffi::im-decorrelation-transform-struct) 'im.ffi::rank)
     :space (cffi:foreign-enum-keyword
             'im.ffi::decorrelation-space
             (cffi:foreign-slot-value
              pointer '(:struct im.ffi::im-decorrelation-transform-struct)
              'im.ffi::color-space)))))

(defun %write-decorrelation-transform (transform pointer)
  (flet ((doubles (slot values)
           (let ((base (cffi:foreign-slot-pointer
                        pointer '(:struct im.ffi::im-decorrelation-transform-struct) slot)))
             (dotimes (i (length values))
               (setf (cffi:mem-aref base :double i) (aref values i))))))
    (doubles 'im.ffi::matrix (decorrelation-matrix transform))
    (doubles 'im.ffi::offset (decorrelation-offset transform))
    (doubles 'im.ffi::mean (decorrelation-mean transform))
    (doubles 'im.ffi::target (decorrelation-target transform))
    (doubles 'im.ffi::stddev (decorrelation-stddev transform))
    (setf (cffi:foreign-slot-value
           pointer '(:struct im.ffi::im-decorrelation-transform-struct) 'im.ffi::rank)
          (decorrelation-rank transform))
    (setf (cffi:foreign-slot-value
           pointer '(:struct im.ffi::im-decorrelation-transform-struct) 'im.ffi::color-space)
          (%decorrelation-space-value (decorrelation-space transform)))))

(defparameter *decorrelation-lab-spaces*
  '(:decorrelation-space-lab :decorrelation-space-lds
    :decorrelation-space-lre :decorrelation-space-lbk
    :decorrelation-space-lye)
  "The spaces that route through CIE L*a*b*.")

(defun %check-decorrelation-normalized (image space)
  "Refuse an unnormalized real image in an L*a*b* space.

L*a*b* is defined over 0-1, and IM's conversion saturates outside it, so a
float image carrying 0-255 comes back a flat single colour rather than an
error -- the failure looks like a broken operation instead of a misuse. There
is no way for the C layer to tell an unnormalized image from a legitimately
bright one, so the check belongs here, and only in the case that would
otherwise fail silently."
  (when (and (member space *decorrelation-lab-spaces*)
             (member (data-type image) '(:data-type-float :data-type-double)))
    (loop for plane below 3
          for stats = (statistics image plane)
          when (or (> (getf stats :max) 1.0d0) (< (getf stats :min) 0.0d0))
            do (cl:error 'im-error
                         :detail (format nil
                                         "~A needs a real image normalized to 0-1; plane ~D spans ~,4F to ~,4F"
                                         space plane
                                         (getf stats :min) (getf stats :max))))))

(defun decorrelation-fit (image &key (space :decorrelation-space-lds) (scale 1.0d0) mask)
  "Fit a decorrelation stretch to IMAGE without applying it.

MASK, when given, is an IM_BINARY or IM_GRAY byte image of the same size, and
only the pixels where it is non-zero are measured. That is the workflow the
compute/apply split exists for: fit to one patch of pigment, apply to the
whole frame, and get the same colours out of every image in a series.

Returns a DECORRELATION-TRANSFORM."
  (%check-decorrelation-normalized image space)
  (cffi:with-foreign-object
      (transform '(:struct im.ffi::im-decorrelation-transform-struct))
    (check-operation "decorrelation-fit"
      (not (zerop (im.ffi::%im-process-decorrelation-calc-transform
                   (handle image)
                   (%decorrelation-space-value space)
                   (coerce scale 'double-float)
                   (cffi:null-pointer)
                   (if mask (handle mask) (cffi:null-pointer))
                   transform))))
    (%read-decorrelation-transform transform)))

(defun decorrelation-apply (src dst transform)
  "Apply a fitted TRANSFORM to SRC, writing DST. SRC and DST may be the same."
  (cffi:with-foreign-object
      (foreign '(:struct im.ffi::im-decorrelation-transform-struct))
    (%write-decorrelation-transform transform foreign)
    (check-operation "decorrelation-apply"
      (not (zerop (im.ffi::%im-process-decorrelation-apply-transform
                   (handle src) (handle dst) foreign))))))

(defun decorrelation-stretch (src dst &key (space :decorrelation-space-lds) (scale 1.0d0))
  "Decorrelation stretch SRC into DST. SRC and DST may be the same image.

Equivalent to DECORRELATION-FIT over the whole image followed by
DECORRELATION-APPLY, and the thing to reach for when the transform itself is
of no interest."
  (%check-decorrelation-normalized src space)
  (check-operation "decorrelation-stretch"
    (not (zerop (im.ffi::%im-process-decorrelation-stretch
                 (handle src) (handle dst)
                 (%decorrelation-space-value space)
                 (coerce scale 'double-float))))))

;;; Statistics and analysis ---------------------------------------------------

(defun statistics (image &optional (plane 0))
  "Statistics for one PLANE of IMAGE, as a property list.

Returns :MAX, :MIN, :MEAN, :STDDEV, :POSITIVE, :NEGATIVE and :ZEROS."
  (let ((depth (+ (depth image) (if (has-alpha-p image) 1 0))))
    (unless (< -1 plane depth)
      (cl:error 'im-error :detail (format nil "plane ~S out of range" plane)))
    ;; IM fills one struct per plane, so the buffer must be depth-sized even
    ;; when only one plane is wanted.
    (cffi:with-foreign-object (stats '(:struct im.ffi::im-stats-struct) depth)
      (im.ffi::%im-calc-image-statistics (handle image) stats)
      (cffi:with-foreign-slots ((im.ffi::max im.ffi::min im.ffi::positive
                                 im.ffi::negative im.ffi::zeros
                                 im.ffi::mean im.ffi::stddev)
                                (cffi:mem-aptr stats '(:struct im.ffi::im-stats-struct) plane)
                                (:struct im.ffi::im-stats-struct))
        (list :max im.ffi::max :min im.ffi::min :mean im.ffi::mean
              :stddev im.ffi::stddev :positive im.ffi::positive
              :negative im.ffi::negative :zeros im.ffi::zeros)))))

(defun rms-error (image other)
  "Root-mean-square difference between two images of the same shape."
  (cffi:with-foreign-object (result :double)
    (im.ffi::%im-calc-rms-error (handle image) (handle other) result)
    (cffi:mem-ref result :double)))

(defun signal-to-noise-ratio (image noise)
  "Signal-to-noise ratio of IMAGE against a NOISE reference, in decibels."
  (cffi:with-foreign-object (result :double)
    (im.ffi::%im-calc-snr (handle image) (handle noise) result)
    (cffi:mem-ref result :double)))

(defun count-colors (image)
  "The number of distinct colours in IMAGE."
  (cffi:with-foreign-object (result :unsigned-long)
    (im.ffi::%im-calc-count-colors (handle image) result)
    (cffi:mem-ref result :unsigned-long)))

(defun make-label-image (source)
  "A destination image of the type IM's region labelling requires.

IM documents the result of imAnalyzeFindRegions as IM_GRAY/IM_USHORT, and does
not check. Handing it an int image -- the obvious guess, since the values are
region numbers -- produces measurements that look plausible and are not."
  (create-based source
                :color-space :color-space-gray
                :data-type :data-type-ushort))

(defun find-regions (src &optional dst &key (connectivity 8) (touch-border t))
  "Label connected white regions of the binary SRC. Returns (VALUES DST COUNT).

DST must be gray ushort; omit it and one is created. CONNECTIVITY is 4 or 8.
When TOUCH-BORDER is false, regions running off the edge are excluded, which
is usually right because their true area cannot be known.

Background is region 0 and is not counted, so the measurement arrays are
indexed 0..COUNT-1 for regions 1..COUNT."
  (let ((destination (or dst (make-label-image src))))
    (unless (and (eq :color-space-gray (color-space destination))
                 (eq :data-type-ushort (data-type destination)))
      (cl:error 'data-error
                :detail (format nil "region labelling needs a gray ushort destination, got ~(~A~) ~(~A~)"
                                (color-space destination) (data-type destination))))
    ;; The return value is the cancellation flag; the count is an out-parameter.
    (cffi:with-foreign-object (count :int)
      (check-operation "find-regions"
        (not (zerop (im.ffi::%im-analyze-find-regions
                     (handle src) (handle destination) connectivity
                     (if touch-border 1 0) count))))
      (values destination (cffi:mem-ref count :int)))))

;;; Watershed segmentation ----------------------------------------------------
;;;
;;; What FIND-REGIONS cannot do. Two objects that touch are one connected
;;; region, and no amount of labelling makes them two; a watershed floods the
;;; image from markers and splits the pair along the neck between them.
;;;
;;; Like the denoising filters above, these check their arguments rather than
;;; passing a precondition violation through as a cancellation -- a marker
;;; image of the wrong data type otherwise arrives as OPERATION-ABORTED.

(defun %check-label-image (image what role)
  (unless (and (eq :color-space-gray (color-space image))
               (eq :data-type-ushort (data-type image)))
    (cl:error 'data-error
              :detail (format nil "~A needs a gray ushort ~A, got ~(~A~) ~(~A~)"
                              what role (color-space image) (data-type image)))))

(defun %check-connectivity (connectivity what)
  (unless (member connectivity '(4 8))
    (cl:error 'data-error
              :detail (format nil "~A connectivity must be 4 or 8, got ~S"
                              what connectivity))))

(defun %check-same-size (a b what)
  (unless (and (= (width a) (width b)) (= (height a) (height b)))
    (cl:error 'data-error
              :detail (format nil "~A needs images of the same size, got ~Dx~D and ~Dx~D"
                              what (width a) (height a) (width b) (height b)))))

(defun watershed (src markers dst &key (connectivity 8) (mark-lines nil))
  "Flood SRC from the labelled MARKERS, writing one label per pixel into DST.

SRC is read as a relief map and flooded lowest ground first, so every pixel
joins the marker whose water reached it. LOW values are flooded first, which
means the basins have to be the features of interest -- NEGATIVE the image
first if they are not, or the segmentation comes out inside out.

SRC is one-plane gray of any real type. MARKERS and DST are gray ushort,
MARKERS labelled as FIND-REGIONS labels with 0 for unmarked; DST may be the
same image as MARKERS. With MARK-LINES true a pixel two basins reach at once
is left 0 and belongs to neither, drawing one-pixel watershed lines.

A region no marker seeds is never labelled: this segments the markers given,
it does not find them. WATERSHED-SEGMENT is the usual way to get markers."
  (%check-same-size src markers "watershed")
  (%check-same-size src dst "watershed")
  (unless (= 1 (depth src))
    (cl:error 'data-error
              :detail (format nil "watershed needs a one-plane relief image, got ~D planes"
                              (depth src))))
  (%check-label-image markers "watershed" "marker image")
  (%check-label-image dst "watershed" "destination")
  (%check-connectivity connectivity "watershed")
  (check-operation "watershed"
    (not (zerop (im.ffi::%im-process-watershed
                 (handle src) (handle markers) (handle dst)
                 connectivity (if mark-lines 1 0)))))
  dst)

(defun watershed-segment (src &optional dst &key (connectivity 8) (mark-lines nil))
  "Split touching objects in the binary SRC and label them. Returns (VALUES DST COUNT).

The same call shape and the same kind of result as FIND-REGIONS -- gray ushort
labels, one per object, which every REGION-* measurement below reads directly
-- but objects that touch come out as separate labels rather than as one
region. IM does it by distance transform, regional maxima for the centres, and
a watershed of the negated distance map seeded from those.

Objects touching the border are always included, unlike FIND-REGIONS, which
can be asked to drop them. MARK-LINES leaves a one-pixel gap of 0 between
objects.

Its characteristic failure is worth knowing: a strongly concave object can
carry more than one distance maximum and be split in two, and nothing in the
output says so. Convex objects of similar size separate cleanly."
  (unless (eq :color-space-binary (color-space src))
    (cl:error 'data-error
              :detail (format nil "watershed-segment needs a binary source, got ~(~A~)"
                              (color-space src))))
  (let ((destination (or dst (make-label-image src))))
    (%check-same-size src destination "watershed-segment")
    (%check-label-image destination "watershed-segment" "destination")
    (%check-connectivity connectivity "watershed-segment")
    (cffi:with-foreign-object (count :int)
      (check-operation "watershed-segment"
        (not (zerop (im.ffi::%im-process-watershed-segment
                     (handle src) (handle destination)
                     connectivity (if mark-lines 1 0) count))))
      (values destination (cffi:mem-ref count :int)))))

;;; Region measurement ------------------------------------------------------
;;;
;;; Every REGION-* function takes the count separately from the image, because
;;; nothing in an imImage records how many regions it carries -- the count
;;; comes back from FIND-REGIONS or WATERSHED-SEGMENT and it is the caller's
;;; job to keep the two together.
;;;
;;; A count BELOW the highest label measures the first REGION-COUNT regions
;;; and ignores the rest, which is a supported way to ask for a prefix. It was
;;; not always: up to tecgraf-im v2.2.0 the six older measurements indexed
;;; their output arrays by label with no range check, so a short count wrote
;;; past the end. The binding carried a scan for the highest label to refuse
;;; that; v2.2.1 range-checks in C, so the scan is gone and the call is
;;; legal. See the minimum version in README.md.

(defun region-areas (labelled region-count)
  "A vector of pixel areas, one per region, for a LABELLED image.

A REGION-COUNT below the highest label measures the first REGION-COUNT
regions and ignores the rest."
  (cffi:with-foreign-object (areas :int region-count)
    (im.ffi::%im-analyze-measure-area (handle labelled) areas region-count)
    (let ((result (make-array region-count)))
      (dotimes (i region-count result)
        (setf (aref result i) (cffi:mem-aref areas :int i))))))

(defun region-centroids (labelled region-count)
  "A vector of (X . Y) centroids, one per region.

IM computes the areas itself when none are supplied, which is why there is no
area argument here -- passing NULL is the documented way to ask for that.

cx and cy are double*, not float*. Reading them as single floats returned
values like 4.07e9 paired with 4.89e-24: the two halves of one double, read as
two floats."
  (cffi:with-foreign-objects ((cx :double region-count)
                              (cy :double region-count))
    (im.ffi::%im-analyze-measure-centroid
     (handle labelled) (cffi:null-pointer) region-count cx cy)
    (let ((result (make-array region-count)))
      (dotimes (i region-count result)
        (setf (aref result i)
              (cons (cffi:mem-aref cx :double i) (cffi:mem-aref cy :double i)))))))

;;; Shape and intensity measurement -------------------------------------------
;;;
;;; Each returns a vector of plists, one per region, indexed 0..REGION-COUNT-1
;;; for regions 1..REGION-COUNT -- the same indexing REGION-AREAS uses, and the
;;; same as the C arrays underneath. A plist rather than several parallel
;;; vectors because these measurements are read together: the four numbers a
;;; Feret measurement produces are one fact about one region.
;;;
;;; The label image is checked here for the reason the denoising filters are:
;;; every one of these C functions reports a label image of the wrong type by
;;; returning the counter's abort value, so without the check a byte image
;;; arrives as "operation cancelled".

(defun %measurements (region-count &rest fields)
  "A vector of plists built from (KEYWORD POINTER CFFI-TYPE) triples."
  (let ((result (make-array region-count)))
    (dotimes (i region-count result)
      (setf (aref result i)
            (loop for (key pointer type) in fields
                  append (list key (cffi:mem-aref pointer type i)))))))

(defun region-bounding-boxes (labelled region-count)
  "A vector of (:XMIN :XMAX :YMIN :YMAX) plists, one per region.

The box is inclusive, so its width is XMAX-XMIN+1. A label that does not occur
in the image reports an empty box -- xmin=ymin=0 and xmax=ymax=-1 -- rather
than a box of negative width."
  (%check-label-image labelled "region-bounding-boxes" "label image")
  (if (zerop region-count)
      #()
      (cffi:with-foreign-objects ((xmin :int region-count) (xmax :int region-count)
                                  (ymin :int region-count) (ymax :int region-count))
        (check-operation "region-bounding-boxes"
          (not (zerop (im.ffi::%im-analyze-measure-bounding-box
                       (handle labelled) region-count xmin xmax ymin ymax))))
        (%measurements region-count
                       (list :xmin xmin :int) (list :xmax xmax :int)
                       (list :ymin ymin :int) (list :ymax ymax :int)))))

(defun region-convex-hulls (labelled region-count)
  "A vector of (:AREA :PERIMETER) plists for each region's convex hull.

Both are geometric: :AREA is the area of the polygon through the pixel
CENTRES, not a count of pixels, so a solid 8x8 block of 64 pixels has a hull
of 49. That is why solidity is not computed here even though IM's own header
gives the formula -- REGION-AREAS counts pixels, and the ratio of the two
exceeds 1 on anything small, which is a value solidity cannot take. Compare
like with like before dividing.

A region of fewer than three non-collinear pixels has a degenerate hull and
reports zero area."
  (%check-label-image labelled "region-convex-hulls" "label image")
  (if (zerop region-count)
      #()
      (cffi:with-foreign-objects ((area :double region-count)
                                  (perimeter :double region-count))
        (check-operation "region-convex-hulls"
          (not (zerop (im.ffi::%im-analyze-measure-convex-hull
                       (handle labelled) region-count area perimeter))))
        (%measurements region-count
                       (list :area area :double)
                       (list :perimeter perimeter :double)))))

(defun region-feret-diameters (labelled region-count)
  "A vector of (:MAX :MAX-ANGLE :MIN :MIN-ANGLE) plists, one per region.

:MAX is the caliper length, the largest distance between any two points of the
region. :MIN is the smallest width over all directions, which is NOT the
shortest distance between two hull points -- that is usually the length of one
short hull edge and says nothing about the shape's width.

Angles are in degrees in [0, 180), anticlockwise from the x axis: a diameter
has no direction, so the range is half a turn rather than a whole one.

These are not the principal axes. The principal axes are moments of the filled
region and are pulled by where its mass sits; Feret diameters are extents of
the outline, decided by the two or three pixels furthest apart."
  (%check-label-image labelled "region-feret-diameters" "label image")
  (if (zerop region-count)
      #()
      (cffi:with-foreign-objects ((max-feret :double region-count)
                                  (max-angle :double region-count)
                                  (min-feret :double region-count)
                                  (min-angle :double region-count))
        (check-operation "region-feret-diameters"
          (not (zerop (im.ffi::%im-analyze-measure-feret
                       (handle labelled) region-count
                       max-feret max-angle min-feret min-angle))))
        (%measurements region-count
                       (list :max max-feret :double)
                       (list :max-angle max-angle :double)
                       (list :min min-feret :double)
                       (list :min-angle min-angle :double)))))

(defun region-intensities (labelled image region-count &key (plane 0))
  "A vector of (:MIN :MAX :MEAN :STDDEV :SUM) plists over IMAGE under each region.

Every other measurement here reads the label image alone and so can only
describe a region's shape. This is the one that says how bright a region is,
which in most of the fields that count objects is the measurement itself.
:SUM is the integrated density.

IMAGE is any real image of LABELLED's width and height, and PLANE selects
which of its planes to measure. :STDDEV divides by n-1, matching STATISTICS; a
one-pixel region reports 0, and a region with no pixels reports zeros
throughout."
  (%check-label-image labelled "region-intensities" "label image")
  (%check-same-size labelled image "region-intensities")
  (let ((depth (+ (depth image) (if (has-alpha-p image) 1 0))))
    (unless (< -1 plane depth)
      (cl:error 'im-error
                :detail (format nil "plane ~S out of range for a ~D-plane image"
                                plane depth))))
  (if (zerop region-count)
      #()
      (cffi:with-foreign-objects ((minimum :double region-count)
                                  (maximum :double region-count)
                                  (mean :double region-count)
                                  (stddev :double region-count)
                                  (sum :double region-count))
        (check-operation "region-intensities"
          (not (zerop (im.ffi::%im-analyze-measure-intensity
                       (handle labelled) (handle image) plane region-count
                       minimum maximum mean stddev sum))))
        (%measurements region-count
                       (list :min minimum :double) (list :max maximum :double)
                       (list :mean mean :double) (list :stddev stddev :double)
                       (list :sum sum :double)))))
