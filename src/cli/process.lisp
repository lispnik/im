;;;; src/cli/process.lisp — `im process', a composable operation pipeline.
;;;;
;;;; Operations are given as repeated --op arguments rather than as one flag
;;;; each:
;;;;
;;;;     im process in.png out.png --op resize=800x600 --op sobel
;;;;
;;;; because order matters and a flag has none. clingon hands back the values
;;;; of a :list option in the order they appeared, which is exactly the
;;;; pipeline; with one flag per operation the tool would have to invent an
;;;; order, and blur-then-threshold is not threshold-then-blur.

(in-package #:im.cli)

;;; The operation registry ----------------------------------------------------

(defstruct (operation (:conc-name op-))
  name
  argument-syntax
  description
  function)

(defvar *operations* (make-hash-table :test #'equal))

(defmacro define-operation (name (image argument) syntax description &body body)
  "Register an operation for the --op registry.

BODY receives the current IMAGE and the raw ARGUMENT string (or NIL) and
returns the next image. Returning a NEW image is normal -- most IM operations
write into a separate destination, and several change the dimensions -- and
the pipeline destroys whatever it replaces."
  `(setf (gethash ,(string-downcase name) *operations*)
         (make-operation :name ,(string-downcase name)
                         :argument-syntax ,syntax
                         :description ,description
                         :function (lambda (,image ,argument)
                                     (declare (ignorable ,argument))
                                     ,@body))))

;;; Argument parsing ----------------------------------------------------------

(defun require-argument (argument operation syntax)
  (unless (and argument (plusp (length argument)))
    (usage-error "--op ~A needs an argument: ~A=~A" operation operation syntax))
  argument)

(defun parse-number (text what)
  (let ((value (ignore-errors
                (let ((*read-eval* nil))
                  (with-standard-io-syntax
                    (let ((*read-eval* nil))
                      (read-from-string text)))))))
    (unless (realp value)
      (usage-error "~A must be a number, got ~S" what text))
    value))

(defun parse-dimensions (text what)
  "Parse WxH, or Nx / xN to scale one axis, or N% for a proportion."
  (let ((percent (position #\% text)))
    (if percent
        (list :scale (/ (parse-number (subseq text 0 percent) what) 100))
        (let ((x (position #\x text :test #'char-equal)))
          (unless x
            (usage-error "~A must look like WIDTHxHEIGHT or N%, got ~S" what text))
          (let ((w (subseq text 0 x))
                (h (subseq text (1+ x))))
            (list :width (unless (zerop (length w)) (floor (parse-number w what)))
                  :height (unless (zerop (length h)) (floor (parse-number h what)))))))))

(defun target-dimensions (image spec)
  "Resolve a parsed dimension SPEC against IMAGE's own size."
  (if (getf spec :scale)
      (let ((scale (getf spec :scale)))
        (values (max 1 (round (* (im:width image) scale)))
                (max 1 (round (* (im:height image) scale)))))
      (let ((w (getf spec :width))
            (h (getf spec :height)))
        ;; A missing axis keeps the aspect ratio rather than the original
        ;; size, which is what `--op resize=800x' is asking for.
        (cond ((and w h) (values w h))
              (w (values w (max 1 (round (* (im:height image)
                                            (/ w (im:width image)))))))
              (h (values (max 1 (round (* (im:width image)
                                          (/ h (im:height image))))) h))
              (t (usage-error "resize needs at least one of WIDTH or HEIGHT"))))))

(defun split-commas (text)
  (loop with start = 0
        for comma = (position #\, text :start start)
        collect (subseq text start comma)
        while comma do (setf start (1+ comma))))

(defun keyword-for-color-space (name)
  (let ((key (intern (string-upcase (format nil "COLOR-SPACE-~A" name)) :keyword)))
    (unless (ignore-errors (cffi:foreign-enum-value 'im.ffi::color-space key))
      (usage-error "unknown colour space ~S; try rgb, gray, binary, map, cmyk, ycbcr, lab, luv or xyz"
                   name))
    key))

(defun keyword-for-data-type (name)
  (let ((key (intern (string-upcase (format nil "DATA-TYPE-~A" name)) :keyword)))
    (unless (ignore-errors (cffi:foreign-enum-value 'im.ffi::data-type key))
      (usage-error "unknown data type ~S; try byte, short, ushort, int, float, double, cfloat or cdouble"
                   name))
    key))

;;; Geometry ------------------------------------------------------------------

(define-operation "resize" (image argument) "WxH | Nx | xN | P%"
  "Resample to a new size, keeping the aspect ratio if one axis is omitted"
  (multiple-value-bind (width height)
      (target-dimensions image (parse-dimensions
                                (require-argument argument "resize" "WxH")
                                "resize"))
    (let ((destination (im:create-based image :width width :height height)))
      (im:resize image destination)
      destination)))

(define-operation "crop" (image argument) "WxH+X+Y"
  "Cut out a rectangle; the offset is from the bottom-left, as IM stores images"
  (let* ((text (require-argument argument "crop" "WxH+X+Y"))
         (plus (position #\+ text)))
    (unless plus
      (usage-error "crop must look like WIDTHxHEIGHTxX+Y, got ~S" text))
    (let* ((size (parse-dimensions (subseq text 0 plus) "crop"))
           (offsets (split-commas (substitute #\, #\+ (subseq text (1+ plus)))))
           (x (floor (parse-number (or (first offsets) "0") "crop x")))
           (y (floor (parse-number (or (second offsets) "0") "crop y")))
           (destination (im:create-based image
                                         :width (getf size :width)
                                         :height (getf size :height))))
      (im:crop image destination x y)
      destination)))

(define-operation "rotate" (image argument) "90 | 180 | 270"
  "Rotate by a right angle"
  (let ((degrees (floor (parse-number (require-argument argument "rotate" "90") "rotate"))))
    (case (mod degrees 360)
      (0 image)
      (180 (let ((destination (im:create-based image)))
             (im:rotate-180 image destination)
             destination))
      ((90 270)
       (let ((destination (im:create-based image
                                           :width (im:height image)
                                           :height (im:width image))))
         (im:rotate-90 image destination (if (= 90 (mod degrees 360)) 1 -1))
         destination))
      (t (usage-error "rotate accepts 90, 180 or 270, got ~D" degrees)))))

(define-operation "mirror" (image argument) nil
  "Reflect left to right"
  (let ((destination (im:create-based image)))
    (im:mirror image destination)
    destination))

(define-operation "flip" (image argument) nil
  "Reflect top to bottom"
  (let ((destination (im:create-based image)))
    (im:flip image destination)
    destination))

;;; Colour and depth ----------------------------------------------------------

(define-operation "colorspace" (image argument) "rgb|gray|binary|..."
  "Convert to another colour space"
  (let* ((space (keyword-for-color-space
                 (require-argument argument "colorspace" "gray")))
         (destination (im:create-based image :color-space space)))
    (im:convert-color-space image destination)
    destination))

(define-operation "depth" (image argument) "byte|ushort|float|..."
  "Convert to another sample type"
  (let* ((type (keyword-for-data-type (require-argument argument "depth" "byte")))
         (destination (im:create-based image :data-type type)))
    (im:convert-data-type image destination)
    destination))

(define-operation "negative" (image argument) nil
  "Invert the samples"
  (let ((destination (im:create-based image)))
    (im:negative image destination)
    destination))

;;; Filtering -----------------------------------------------------------------

(define-operation "gaussian" (image argument) "STDDEV"
  "Gaussian blur"
  (let ((destination (im:create-based image)))
    (im:convolve-gaussian image destination
                          (parse-number (require-argument argument "gaussian" "2.0")
                                        "gaussian stddev"))
    destination))

(define-operation "median" (image argument) "SIZE"
  "Median filter over a SIZExSIZE neighbourhood"
  (let ((destination (im:create-based image)))
    (im:convolve-median image destination
                        (floor (parse-number (require-argument argument "median" "3")
                                             "median size")))
    destination))

(define-operation "sobel" (image argument) nil
  "Sobel edge magnitude"
  (let ((destination (im:create-based image)))
    (im:convolve-sobel image destination)
    destination))

(define-operation "prewitt" (image argument) nil
  "Prewitt edge magnitude"
  (let ((destination (im:create-based image)))
    (im:convolve-prewitt image destination)
    destination))

(define-operation "canny" (image argument) "STDDEV"
  "Canny edge detection"
  (let ((destination (im:create-based image)))
    (im:canny image destination
              (parse-number (require-argument argument "canny" "1.4") "canny stddev"))
    destination))

(define-operation "unsharp" (image argument) "STDDEV,AMOUNT,THRESHOLD"
  "Unsharp mask"
  (let* ((parts (split-commas (require-argument argument "unsharp" "2.0,1.0,0.0")))
         (destination (im:create-based image)))
    (im:unsharp image destination
                (parse-number (or (first parts) "2.0") "unsharp stddev")
                (parse-number (or (second parts) "1.0") "unsharp amount")
                (parse-number (or (third parts) "0.0") "unsharp threshold"))
    destination))

;;; Thresholding and morphology -----------------------------------------------

(define-operation "threshold" (image argument) "LEVEL | otsu"
  "Binarise, either at a fixed level or at the one Otsu's method picks"
  (let* ((text (require-argument argument "threshold" "otsu"))
         ;; Thresholding needs a gray source and produces a binary image, so
         ;; convert first rather than making the user chain three operations.
         (gray (if (eq :color-space-gray (im:color-space image))
                   image
                   (let ((g (im:create-based image :color-space :color-space-gray)))
                     (im:convert-color-space image g)
                     g)))
         (destination (im:create-based gray :color-space :color-space-binary)))
    (unwind-protect
         (if (string-equal text "otsu")
             (let ((level (im:threshold-otsu gray destination)))
               (verbose "~&Otsu chose level ~D~%" level))
             (im:threshold gray destination (parse-number text "threshold level")))
      (unless (eq gray image) (im:destroy gray)))
    destination))

(macrolet ((define-morphology (name function description)
             `(define-operation ,name (image argument) "SIZE"
                ,description
                (let ((destination (im:create-based image))
                      (size (if argument
                                (floor (parse-number argument ,(format nil "~A size" name)))
                                3)))
                  (,function image destination :size size)
                  destination))))
  (define-morphology "erode" im:morph-erode "Morphological erosion")
  (define-morphology "dilate" im:morph-dilate "Morphological dilation")
  (define-morphology "open" im:morph-open "Erode then dilate")
  (define-morphology "close" im:morph-close "Dilate then erode"))

;;; Frequency domain ----------------------------------------------------------

(define-operation "spectrum" (image argument) nil
  "Fourier magnitude spectrum, centred and scaled for viewing"
  ;; The transform itself is FFT -> swap-quadrants -> magnitude. Exposed as one
  ;; operation because the intermediate is a complex image, and no image format
  ;; can store one -- `--op fft' on its own would produce something unsaveable.
  ;; IM:FFT and IM:IFFT are there in the library API for callers who want the
  ;; coefficients.
  (let ((gray (if (eq :color-space-gray (im:color-space image))
                  image
                  (let ((g (im:create-based image :color-space :color-space-gray)))
                    (im:convert-color-space image g)
                    g))))
    (unwind-protect
         (let ((spectrum (im:create-based gray :data-type :data-type-cfloat)))
           (unwind-protect
                (progn
                  (im:fft gray spectrum)
                  (im:swap-quadrants spectrum)
                  (let ((viewable (im:create-based gray :data-type :data-type-byte)))
                    ;; Magnitudes span many orders of magnitude, so a linear
                    ;; cast leaves everything but the DC term black. IM's
                    ;; logarithmic gamma is what makes a spectrum legible.
                    (im:convert-data-type spectrum viewable
                                          :complex-part :magnitude
                                          :gamma -10.0d0
                                          :cast-mode :min-max)
                    viewable))
             (im:destroy spectrum)))
      (unless (eq gray image) (im:destroy gray)))))

;;; Driving the pipeline ------------------------------------------------------

(defun parse-op-spec (spec)
  "Split `name=argument' into the operation and its argument."
  (let* ((equals (position #\= spec))
         (name (string-downcase (if equals (subseq spec 0 equals) spec)))
         (argument (when equals (subseq spec (1+ equals))))
         (operation (gethash name *operations*)))
    (unless operation
      (usage-error "unknown operation ~S. Try `im process --list-ops'." name))
    (values operation argument)))

(defun call-with-progress (thunk)
  "Run THUNK with an IM progress callback attached, if --verbose is on.

Doubles as the only exercise the cancellation path gets outside the test
suite: the callback returns true throughout, but the machinery that would turn
a false return into IM:OPERATION-ABORTED is the same."
  (if *verbose*
      (let ((last -1))
        (im:with-progress ((lambda (id text progress)
                             (declare (ignore id))
                             (let ((decile (floor progress 100)))
                               (when (and (<= 0 progress 1000) (/= decile last))
                                 (setf last decile)
                                 (format *error-output* "~&  ~3D%~@[ ~A~]~%"
                                         (floor progress 10) text)
                                 (finish-output *error-output*)))
                             t))
          (funcall thunk)))
      (funcall thunk)))

(defun run-pipeline (image specs)
  "Apply SPECS to IMAGE in order, returning the final image.

Each step may return a new image; the previous one is destroyed as soon as it
has been replaced, so a long pipeline holds two images rather than all of
them. IMAGE itself is owned by the caller and never destroyed here."
  (let ((current image))
    (dolist (spec specs current)
      (multiple-value-bind (operation argument) (parse-op-spec spec)
        (verbose "~&~A~@[=~A~]~%" (op-name operation) argument)
        (let ((next (call-with-progress
                     (lambda () (funcall (op-function operation) current argument)))))
          (unless (eq next current)
            (unless (eq current image) (im:destroy current))
            (setf current next)))))))

(defun list-operations ()
  (emit-table
   (loop for name in (sort (alexandria:hash-table-keys *operations*) #'string<)
         for operation = (gethash name *operations*)
         collect (list name
                       (or (op-argument-syntax operation) "")
                       (op-description operation)))
   :headers '("OP" "ARGUMENT" "DESCRIPTION")))

(defun process/options ()
  (list
   (clingon:make-option
    :list :long-name "op" :short-name #\o :key :ops
    :description "An operation to apply; repeat, and they run in order")
   (clingon:make-option
    :flag :long-name "list-ops" :key :list-ops
    :description "List the available operations and exit")
   (clingon:make-option
    :string :long-name "format" :key :format
    :description "Output format name; default is guessed from the extension")
   (clingon:make-option
    :string :long-name "compression" :key :compression
    :description "Output compression; see `im formats --compressions'")
   (clingon:make-option
    :integer :long-name "frame" :key :frame :initial-value 0
    :description "Which frame of a multi-image input to read")))

(defun process/handler (command)
  (apply-global-options command)
  (when (clingon:getopt command :list-ops)
    (list-operations)
    (return-from process/handler))
  (let ((arguments (clingon:command-arguments command))
        (ops (clingon:getopt command :ops)))
    (unless (= 2 (length arguments))
      (usage-error "process needs an input and an output file. Try `im process --help'."))
    (when (null ops)
      (usage-error "process needs at least one --op. Try `im process --list-ops'."))
    (destructuring-bind (input output) arguments
      (im:with-image (source (im:load (pathname input)
                                      :index (clingon:getopt command :frame)))
        (let ((result (run-pipeline source ops)))
          (unwind-protect
               (progn
                 (im:save result (pathname output)
                          :format (clingon:getopt command :format)
                          :compression (clingon:getopt command :compression))
                 (emit (list :input (pathname input)
                             :output (pathname output)
                             :operations ops
                             :width (im:width result)
                             :height (im:height result)
                             :color-space (im:color-space result)
                             :data-type (im:data-type result))))
            (unless (eq result source) (im:destroy result))))))))

(register-subcommand
 (clingon:make-command
  :name "process"
  :description "Apply a pipeline of image operations"
  :usage "INPUT OUTPUT --op NAME[=ARG] [--op ...]"
  :options (process/options)
  :handler (guarded #'process/handler)))
