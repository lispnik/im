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
          region-areas
          region-centroids))

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

(defun region-areas (labelled region-count)
  "A vector of pixel areas, one per region, for a LABELLED image."
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
