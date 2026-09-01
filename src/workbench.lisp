;;;; src/workbench.lisp — a small toolkit for working with images at the REPL.
;;;;
;;;; The processing operations in this binding are written the way IM writes
;;;; them: (OPERATION SRC DST ...), where the caller allocates DST and the
;;;; result is a side effect on it. That is the right primitive, and the wrong
;;;; shape for a REPL, where you want to read a transformation left to right
;;;; and see the result. This file adds the missing functional layer:
;;;;
;;;;   DERIVE turns an (SRC DST) operation into IMAGE -> fresh IMAGE.
;;;;   GRAYSCALE and RESIZED are the two shape-changing operations pre-wrapped.
;;;;   PIPE threads an image through a series of those, reclaiming intermediates.
;;;;   SHOW displays an image and prints what it is.
;;;;   ENABLE-REPL-IMAGES makes a bare IMAGE result render itself.
;;;;
;;;; None of it is new imaging capability -- it is the existing operations made
;;;; pleasant to compose interactively, on top of DISPLAY.

(in-package #:im)

(export '(pipe
          derive
          grayscale
          resized
          show
          enable-repl-images
          disable-repl-images))

;;; Functional operations -----------------------------------------------------

(defun derive (image operation &rest args)
  "Run OPERATION and return the fresh image it wrote, rather than a destination
the caller had to allocate.

OPERATION is any of this package's (SRC DST . ARGS) processing functions whose
output has the source's geometry, type and colour space -- CONVOLVE-SOBEL,
NEGATIVE, MORPH-ERODE, THRESHOLD-OTSU and most of the rest. The destination is
built with CREATE-BASED, OPERATION fills it, and it comes back as a value:

  (im:derive photo #'im:convolve-sobel)
  (im:derive photo #'im:morph-erode kernel)

Operations that change the geometry or type build their own destination;
GRAYSCALE and RESIZED wrap the common ones, and anything else is a one-line
lambda over CREATE / CREATE-BASED."
  (let ((dst (create-based image)))
    (apply operation image dst args)
    dst))

(defun grayscale (image)
  "A fresh greyscale copy of IMAGE.

Already-grey images are DUPLICATEd rather than returned as-is, so the result is
always a new image PIPE may reclaim without touching the caller's."
  (if (eq :color-space-gray (color-space image))
      (duplicate image)
      (let ((dst (create-based image :color-space :color-space-gray)))
        (convert-color-space image dst)
        dst)))

(defun resized (image &key width height scale (order 1))
  "A fresh resized copy of IMAGE.

Give WIDTH and/or HEIGHT in pixels, or a SCALE factor for both. With only one
of WIDTH/HEIGHT the other follows to preserve the aspect ratio. ORDER is the
interpolation order RESIZE takes: 0 nearest, 1 bilinear, 3 bicubic."
  (let* ((w0 (width image))
         (h0 (height image))
         (w (cond (width width)
                  (scale (max 1 (round (* w0 scale))))
                  (height (max 1 (round (* w0 (/ height h0)))))
                  (t w0)))
         (h (cond (height height)
                  (scale (max 1 (round (* h0 scale))))
                  (width (max 1 (round (* h0 (/ width w0)))))
                  (t h0)))
         (dst (create-based image :width w :height h)))
    (resize image dst order)
    dst))

;;; The pipeline ---------------------------------------------------------------

(defun %run-pipe (image stages)
  "Thread IMAGE through STAGES, a list of one-argument functions each returning
an image, reclaiming every intermediate as the next stage consumes it.

DESTROY is idempotent and finalizer-backed, so freeing intermediates eagerly
is a promptness optimisation, not a correctness requirement -- and it is
guarded so the caller's input (which the caller still owns) and any stage that
returns its own argument are never freed."
  (let ((current image))
    (dolist (stage stages current)
      (let ((next (funcall stage current)))
        (unless (or (eq current image) (eq current next))
          (when (and (imagep current) (not (destroyed-p current)))
            (ignore-errors (destroy current))))
        (setf current next)))))

(defmacro pipe (image &body stages)
  "Thread IMAGE through STAGES left to right and return the final image.

Each stage is a function of one image returning a fresh image. A bare function
name or #'FUNCTION is called on the current image; a compound form has the
image spliced in as its first argument, so (RESIZED :scale 0.5) runs as
(RESIZED <image> :scale 0.5):

  (im:pipe (im:load #p\"photo.jpg\")
           (resized :scale 0.5)
           #'grayscale
           (derive #'im:convolve-sobel)
           #'show)

Intermediate images are reclaimed as soon as the following stage has used
them; the image passed in and the image returned are left for the caller to
own and DESTROY (or wrap in WITH-IMAGE). A stage that neither allocates nor
returns a fresh image -- one that mutates in place and returns its argument --
is fine: the reclaim step skips an image a stage handed straight back."
  (let ((functions
          (mapcar (lambda (stage)
                    (cond
                      ((and (consp stage) (eq (first stage) 'function))
                       stage)                       ; #'foo
                      ((consp stage)                ; (f a b) -> thread-first
                       (let ((image-arg (gensym "IMG")))
                         `(lambda (,image-arg) (,(first stage) ,image-arg ,@(rest stage)))))
                      (t `(function ,stage))))      ; bare foo
                  stages)))
    `(%run-pipe ,image (list ,@functions))))

;;; Showing --------------------------------------------------------------------

(defun show (image &key (stream *standard-output*) (display t))
  "Print what IMAGE is -- geometry and per-plane statistics -- to STREAM, show
it with DISPLAY unless DISPLAY is NIL, and return IMAGE.

Returning the image makes SHOW a tap inside a PIPE: drop it between stages to
see an intermediate without breaking the chain. The statistics are STATISTICS'
own, one line per plane.

DISPLAY errors are swallowed so SHOW is useful in a bare REPL too, where it
degrades to the printed summary; call DISPLAY directly if you want the
DISPLAY-UNAVAILABLE condition."
  (format stream "~&~Dx~D ~(~A~) ~(~A~)~@[ +alpha~]~%"
          (width image) (height image)
          (color-space image) (data-type image) (has-alpha-p image))
  (dotimes (plane (depth image))
    (let ((s (statistics image plane)))
      (format stream "  plane ~D  min ~,4G  max ~,4G  mean ~,4G  stddev ~,4G~%"
              plane (getf s :min) (getf s :max) (getf s :mean) (getf s :stddev))))
  (when display (ignore-errors (display image)))
  image)

;;; Rendering a bare result at the REPL ---------------------------------------
;;;
;;; SLIME lets a contrib replace how REPL results are sent to Emacs, through
;;; SWANK-REPL:*SEND-REPL-RESULTS-FUNCTION*. We wrap it: an IM:IMAGE result is
;;; DISPLAYed (the same slime-media path DISPLAY already uses), everything else
;;; goes to the function that was there before. SLY's mrepl has no equivalent
;;; hook, so under SLY this reports that and you use SHOW or DISPLAY explicitly.
;;;
;;; This is the one piece here that cannot be exercised without a live Emacs on
;;; the other end, so it is opt-in and defensive: it only ever touches SWANK if
;;; SWANK is loaded, and DISABLE-REPL-IMAGES puts back exactly what it found.

(defvar *previous-repl-results-function* nil
  "What SWANK-REPL:*SEND-REPL-RESULTS-FUNCTION* held before ENABLE-REPL-IMAGES,
so DISABLE-REPL-IMAGES can restore it.")

(defun %repl-results-variable ()
  "The SWANK-REPL:*SEND-REPL-RESULTS-FUNCTION* symbol, or NIL if SLIME's REPL
contrib is not loaded."
  (let ((package (find-package '#:swank-repl)))
    (when package
      (let ((symbol (find-symbol "*SEND-REPL-RESULTS-FUNCTION*" package)))
        (and symbol (boundp symbol) symbol)))))

(defun %send-repl-results-with-images (values)
  "A SWANK-REPL:*SEND-REPL-RESULTS-FUNCTION*: DISPLAY the image results, hand
everything else to the previous function unchanged."
  (if (some #'imagep values)
      (dolist (value values)
        (if (imagep value)
            (ignore-errors (display value))
            (when *previous-repl-results-function*
              (funcall *previous-repl-results-function* (list value)))))
      (when *previous-repl-results-function*
        (funcall *previous-repl-results-function* values))))

(defun enable-repl-images ()
  "Make a bare IM:IMAGE at the SLIME REPL render itself instead of printing
#<IM:IMAGE ...>. Returns T when installed.

Needs the slime-media contrib on the Emacs side, the same prerequisite DISPLAY
documents. Under SLY -- whose mrepl offers no result hook -- or a bare Lisp,
this signals an error naming the limitation; use SHOW or DISPLAY there. Undo
with DISABLE-REPL-IMAGES."
  (let ((variable (%repl-results-variable)))
    (unless variable
      (cl:error 'im-error
                :detail (concatenate
                         'string
                         "REPL image rendering needs SLIME's swank-repl, which is "
                         "not loaded. SLY and a bare Lisp are not supported here "
                         "-- use IM:SHOW or IM:DISPLAY.")))
    (let ((current (symbol-value variable)))
      (unless (eq current '%send-repl-results-with-images)
        (setf *previous-repl-results-function* current)))
    (setf (symbol-value variable) '%send-repl-results-with-images)
    t))

(defun disable-repl-images ()
  "Restore the REPL result handler ENABLE-REPL-IMAGES replaced. Returns T when
there was something to restore."
  (let ((variable (%repl-results-variable)))
    (when (and variable *previous-repl-results-function*)
      (setf (symbol-value variable) *previous-repl-results-function*
            *previous-repl-results-function* nil)
      t)))
