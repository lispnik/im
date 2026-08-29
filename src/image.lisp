;;;; src/image.lisp — the IMAGE class and its lifetime.
;;;;
;;;; The previous binding handed callers a bare foreign pointer from nine
;;;; different constructors and left freeing it to them. There was no
;;;; WITH-IMAGE in the library (only in the test fixtures), no finalizer, and
;;;; no way to tell a live pointer from a freed one -- so the two failure modes
;;;; were a leak and a use-after-free, and neither announced itself.
;;;;
;;;; Here an image is a CLOS object with three overlapping safeguards:
;;;;
;;;;   WITH-IMAGE releases on unwind. This is the intended way.
;;;;   A finalizer releases images that escape one, at GC.
;;;;   DESTROY is idempotent and cancels the finalizer, so the two cannot
;;;;   both run.
;;;;
;;;; The ordering rule that makes this safe: the finalizer must not close over
;;;; the IMAGE object. A finalizer that references the very object whose
;;;; collection triggers it keeps that object alive forever, and TRIVIAL-GARBAGE
;;;; documents this. It closes over the pointer only.

(in-package #:im)

(export '(image
          imagep
          handle
          destroy
          destroyed-p
          with-image
          with-images
          width
          height
          color-space
          data-type
          depth
          has-alpha-p
          line-size
          plane-size
          data-size
          pixel-count
          plane-pointer
          palette-count
          bitmap-p
          create
          create-based
          duplicate
          clone
          clear
          color-mode-config))

(defclass image ()
  ((handle :initarg :handle :accessor %handle
           :documentation "The imImage*, or NIL once destroyed.")
   (finalizer-key :initform nil :accessor %finalizer-key
                  :documentation
                  "A cons cell whose CAR is the pointer the finalizer frees.

Indirection so DESTROY can disarm the finalizer without racing it: setting the
CAR to NIL is a single write, and a finalizer that finds NIL does nothing."))
  (:documentation
   "An IM image: a width by height array of DEPTH planes.

Planes are unpacked and stored bottom-up, always -- that is IM's model, not a
mode. PLANE-POINTER gives the raw foreign storage for one plane; IM's own
documentation argues against per-pixel accessors here, and it is right, so
this binding does not pretend to offer them."))

(defun imagep (object) (typep object 'image))

(defmethod print-object ((image image) stream)
  (print-unreadable-object (image stream :type t :identity nil)
    (if (destroyed-p image)
        (format stream "destroyed")
        (format stream "~Dx~D ~(~A~) ~(~A~)~@[ +alpha~]"
                (width image) (height image)
                (color-space image) (data-type image)
                (has-alpha-p image)))))

;;; Lifetime ------------------------------------------------------------------

(defun %free-handle (key)
  "Release the pointer in KEY, once. Used by both DESTROY and the finalizer."
  (let ((pointer (car key)))
    (when pointer
      (setf (car key) nil)
      (im.ffi::%im-image-destroy pointer))))

(defun wrap-handle (pointer)
  "Wrap a fresh imImage* in an IMAGE, arming its finalizer.

Every constructor funnels through here, so there is exactly one place where
ownership begins."
  (when (or (null pointer) (cffi:null-pointer-p pointer))
    (cl:error 'memory-error :detail "imImage allocation"))
  (let* ((key (list pointer))
         (image (make-instance 'image :handle pointer)))
    (setf (%finalizer-key image) key)
    ;; Closes over KEY, never over IMAGE. See the file header.
    (tg:finalize image (lambda () (%free-handle key)))
    image))

(defun handle (image)
  "The live imImage* behind IMAGE, or signal INVALID-IMAGE.

Every operation goes through this, which is what turns a use-after-free into a
condition with a name."
  (or (%handle image)
      (cl:error 'invalid-image :image image)))

(defun destroyed-p (image)
  (null (%handle image)))

(defun destroy (image)
  "Release IMAGE's foreign storage. Safe to call more than once.

Cancels the finalizer first, so a later GC cannot free the same pointer again."
  (when (%handle image)
    (setf (%handle image) nil)
    (tg:cancel-finalization image)
    (%free-handle (%finalizer-key image)))
  nil)

(defmacro with-image ((var form) &body body)
  "Evaluate BODY with VAR bound to the image FORM returns, destroying it after.

The image is destroyed however BODY leaves -- return, error or throw."
  `(let ((,var ,form))
     (unwind-protect (progn ,@body)
       (when ,var (destroy ,var)))))

(defmacro with-images (bindings &body body)
  "WITH-IMAGE over several bindings, released in reverse order.

Each binding sees the ones before it, so a destination image can be built from
a source's dimensions."
  (if (null bindings)
      `(progn ,@body)
      `(with-image ,(first bindings)
         (with-images ,(rest bindings) ,@body))))

;;; Accessors -----------------------------------------------------------------
;;;
;;; Read from the struct every time rather than cached at construction.
;;; imImageReshape rewrites width, height and every derived field in place, so
;;; a cached copy is stale from that call onward.

(macrolet ((define-slot-reader (lisp-name slot &optional doc)
             `(progn
                (defun ,lisp-name (image)
                  ,@(when doc (list doc))
                  (cffi:foreign-slot-value (handle image)
                                           '(:struct im.ffi::im-image-struct)
                                           ',slot)))))
  (define-slot-reader width im.ffi::width "Number of columns.")
  (define-slot-reader height im.ffi::height "Number of lines.")
  (define-slot-reader depth im.ffi::depth
    "Number of planes, excluding alpha.")
  (define-slot-reader line-size im.ffi::line-size
    "Bytes per line in one plane.")
  (define-slot-reader plane-size im.ffi::plane-size
    "Bytes per plane.")
  (define-slot-reader data-size im.ffi::size
    "Total bytes of image data, across all planes.")
  (define-slot-reader pixel-count im.ffi::count
    "Pixels per plane, i.e. width times height.")
  (define-slot-reader palette-count im.ffi::palette-count
    "Number of palette entries in use. The palette always has 256 allocated."))

(defun color-space (image)
  "The colour space as a keyword, e.g. :COLOR-SPACE-RGB.

IM packs the colour-space enum and three configuration bits into one int. This
returns only the space; COLOR-MODE-CONFIG returns the bits."
  (cffi:foreign-enum-keyword
   'im.ffi::color-space
   (logand (cffi:foreign-slot-value (handle image)
                                    '(:struct im.ffi::im-image-struct)
                                    'im.ffi::color-space)
           #xff)))

(defun color-mode-config (image)
  "The colour mode configuration bits as a list of keywords.

One of :COLOR-MODE-CONFIG-ALPHA, -PACKED and -TOPDOWN. For an IMAGE the last
two are always absent -- imImage is defined as unpacked and bottom-up -- so
this is mostly of interest for values read from a file header."
  (cffi:foreign-bitfield-symbols
   'im.ffi::color-mode-config
   (logand (cffi:foreign-slot-value (handle image)
                                    '(:struct im.ffi::im-image-struct)
                                    'im.ffi::color-space)
           #xff00)))

(defun data-type (image)
  "The sample type as a keyword, e.g. :DATA-TYPE-BYTE."
  (cffi:foreign-enum-keyword
   'im.ffi::data-type
   (cffi:foreign-slot-value (handle image)
                            '(:struct im.ffi::im-image-struct)
                            'im.ffi::data-type)))

(defun has-alpha-p (image)
  (not (zerop (cffi:foreign-slot-value (handle image)
                                       '(:struct im.ffi::im-image-struct)
                                       'im.ffi::has-alpha))))

(defun bitmap-p (image)
  "True when IMAGE can be displayed directly: 8-bit RGB, gray, map or binary."
  (not (zerop (im.ffi::%im-image-is-bitmap (handle image)))))

(defun plane-pointer (image plane)
  "A foreign pointer to PLANE's storage.

Plane 0 also addresses the whole buffer, and plane i is plane 0 offset by
PLANE-SIZE times i -- so this is a pointer into one allocation, not a separate
one, and it must not be freed."
  (let ((depth (+ (depth image) (if (has-alpha-p image) 1 0))))
    (unless (and (integerp plane) (<= 0 plane) (< plane depth))
      (cl:error 'im-error
                :detail (format nil "plane ~S out of range for a ~D-plane image"
                                plane depth))))
  (cffi:mem-aref (cffi:foreign-slot-value (handle image)
                                          '(:struct im.ffi::im-image-struct)
                                          'im.ffi::data)
                 :pointer plane))

;;; Construction --------------------------------------------------------------

(defun create (width height color-space data-type)
  "Allocate a WIDTH by HEIGHT image. Data is cleared.

COLOR-SPACE and DATA-TYPE are keywords: :COLOR-SPACE-RGB, :DATA-TYPE-BYTE."
  (wrap-handle
   (im.ffi::%im-image-create width height
                             (cffi:foreign-enum-value 'im.ffi::color-space color-space)
                             (cffi:foreign-enum-value 'im.ffi::data-type data-type))))

(defun create-based (image &key width height color-space data-type)
  "Allocate an image like IMAGE, overriding whichever parameters are given.

Attributes and the alpha flag are copied. IM spells \"keep the original\" as
-1 for each parameter, which is why every keyword here defaults to NIL rather
than to the source's value: passing the source's value back would be the same
thing, but this way IM does the copying and there is one less place to be
wrong."
  (wrap-handle
   (im.ffi::%im-image-create-based
    (handle image)
    (or width -1)
    (or height -1)
    (if color-space (cffi:foreign-enum-value 'im.ffi::color-space color-space) -1)
    (if data-type (cffi:foreign-enum-value 'im.ffi::data-type data-type) -1))))

(defun duplicate (image)
  "A new image with the same parameters AND a copy of the data."
  (wrap-handle (im.ffi::%im-image-duplicate (handle image))))

(defun clone (image)
  "A new image with the same parameters but uninitialised data."
  (wrap-handle (im.ffi::%im-image-clone (handle image))))

(defun clear (image)
  "Set all data to zero, or to the colour-space's notion of black."
  (im.ffi::%im-image-clear (handle image))
  image)
