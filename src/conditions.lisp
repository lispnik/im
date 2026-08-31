;;;; src/conditions.lisp — the condition hierarchy.
;;;;
;;;; Deliberately free of foreign calls, so it loads before any library is
;;;; open and can be used to report a failure to open one.
;;;;
;;;; Two things about the previous design are fixed here, both of which made
;;;; failures silent rather than loud:
;;;;
;;;;   IM-ERROR was the only condition that inherited from CL:ERROR. The other
;;;;   six -- COUNTER-ABORTED and the five capture conditions -- were bare
;;;;   CONDITIONs, yet five of them were raised with (ERROR 'FOO). Signalling a
;;;;   non-SERIOUS-CONDITION that way is not what it looks like.
;;;;
;;;;   The error code was a slot on one flat class, so a caller who wanted to
;;;;   treat "file not found" differently from "out of memory" had to test
;;;;   (EQ (ERROR-CODE C) :ERROR-CODE-OPEN) inside a handler for every IM
;;;;   failure. Here each imErrorCodes member is its own class, so
;;;;   HANDLER-CASE dispatches on it directly.

(in-package #:im)

(export '(im-condition
          im-error
          error-code
          error-detail
          io-error
          open-error
          access-error
          format-error
          data-error
          compress-error
          memory-error
          invalid-image
          invalid-image-object
          library-not-found
          library-not-found-candidates
          display-unavailable
          operation-aborted
          operation-aborted-operation
          operation-aborted-progress
          capture-error
          no-device-error
          invalid-device-error
          device-connection-error
          device-configuration-error))

;;; Readers are declared ahead of the conditions that fill them in, so each
;;; carries its own documentation. A slot's :DOCUMENTATION describes the slot
;;; and is not what DESCRIBE or a doc generator finds when asked about the
;;; function.

(defgeneric error-code (condition)
  (:documentation
   "The imErrorCodes keyword IM reported -- :ERROR-CODE-OPEN, :ERROR-CODE-MEM
and so on -- or NIL when the failure did not come from a C error code.

Kept alongside the condition class rather than instead of it: the class is
what you dispatch on, this is what you print or log."))

(defgeneric error-detail (condition)
  (:documentation
   "Context the binding added -- the pathname, format or operation involved --
or NIL. IM's error codes carry no payload of their own, so without this a
report can say only that an open failed, not what failed to open."))

(define-condition im-condition (condition)
  ()
  (:documentation "Root of every condition this library signals."))

(define-condition im-error (im-condition cl:error)
  ((code :initarg :code :initform nil :reader error-code
         :documentation "The imErrorCodes keyword, or NIL.")
   (detail :initarg :detail :initform nil :reader error-detail
           :documentation "Pathname, format or operation context, or NIL."))
  (:documentation "Base class for every failure this library reports.")
  (:report (lambda (c stream)
             (format stream "IM error~@[ (~A)~]~@[: ~A~]"
                     (error-code c) (error-detail c)))))

;;; File and format errors ----------------------------------------------------
;;;
;;; One class per imErrorCodes member (im.h:62). IM_ERR_COUNTER is not among
;;; them -- it means the user's own progress callback asked to stop, which is
;;; not an error in the same sense and gets OPERATION-ABORTED below.

(define-condition io-error (im-error) ()
  (:documentation "A file or format operation failed.

Named IO-ERROR rather than FILE-ERROR because CL:FILE-ERROR is inherited into
this package by :USE, and redefining it there is a package-lock violation.
Subclassing CL:FILE-ERROR was the other option and was rejected: its contract
is that FILE-ERROR-PATHNAME returns a pathname, and half of these carry a
format or compression name instead."))

(defmacro %define-file-error (name code report)
  "Define an IM-ERROR subclass for one imErrorCodes member.

Each takes its code as an :INITFORM so a caller can signal it without
restating the keyword, and so the class and the code cannot drift apart."
  `(define-condition ,name (io-error)
     ((code :initform ,code))
     (:report (lambda (c stream)
                (format stream ,report (or (error-detail c) "<unknown>"))))))

(%define-file-error open-error     :error-code-open
                   "Cannot open ~A for reading or writing.")
(%define-file-error access-error   :error-code-access
                   "Cannot access ~A -- the file was opened but reading or writing it failed.")
(%define-file-error format-error   :error-code-format
                   "Invalid or unrecognized file format: ~A.")
(%define-file-error data-error     :error-code-data
                   "Invalid or unsupported image data: ~A.")
(%define-file-error compress-error :error-code-compress
                   "Invalid or unsupported compression: ~A.")
(%define-file-error memory-error   :error-code-mem
                   "Insufficient memory for ~A.")

(defparameter *error-code-classes*
  '((:error-code-open     . open-error)
    (:error-code-access   . access-error)
    (:error-code-format   . format-error)
    (:error-code-data     . data-error)
    (:error-code-compress . compress-error)
    (:error-code-mem      . memory-error))
  "Maps an imErrorCodes keyword to the condition class MAYBE-ERROR signals.

:ERROR-CODE-NONE and :ERROR-CODE-COUNTER are absent on purpose -- the first is
success and the second is a user-requested cancellation, handled by
OPERATION-ABORTED.")

(defun maybe-error (error-code &optional detail)
  "Signal the condition for ERROR-CODE unless it reports success.

DETAIL is the pathname, format name or operation the caller was working on;
it is what the report method prints, and the difference between \"cannot open
<unknown>\" and a message naming the file."
  (case error-code
    (:error-code-none nil)
    (:error-code-counter (signal-operation-aborted detail))
    (t (let ((class (cdr (assoc error-code *error-code-classes*))))
         (if class
             (cl:error class :detail detail)
             ;; A code this binding has no class for: still an error, and
             ;; still worth reporting with the raw keyword rather than
             ;; silently succeeding, which is what an unmatched CASE would do.
             (cl:error 'im-error :code error-code :detail detail))))))

;;; Cancellation --------------------------------------------------------------

(define-condition operation-aborted (im-error)
  ((code :initform :error-code-counter)
   (operation :initarg :operation :initform nil
              :reader operation-aborted-operation
              :documentation "Name of the IM operation that was cancelled.")
   (progress :initarg :progress :initform nil
             :reader operation-aborted-progress
             :documentation
             "Last progress value the counter callback saw, 0-1000, or NIL."))
  (:documentation
   "A long-running operation stopped because a progress callback asked it to.

This replaces the previous COUNTER-ABORTED, which was a bare CONDITION raised
with SIGNAL. With no handler installed, SIGNAL on a non-error condition
returns -- so every cancelled operation used to return NIL and let execution
carry straight on, in a library where the next call would read the
half-written destination image.")
  (:report (lambda (c stream)
             (format stream "IM operation~@[ ~A~] was cancelled~@[ at ~,1F%~]."
                     (operation-aborted-operation c)
                     (let ((p (operation-aborted-progress c)))
                       (when p (/ p 10.0)))))))

(defun signal-operation-aborted (&optional operation progress)
  (cl:error 'operation-aborted :operation operation :progress progress))

;;; Images --------------------------------------------------------------------

(define-condition invalid-image (im-error)
  ((image :initarg :image :initform nil :reader invalid-image-object
          :documentation "The IM:IMAGE whose handle was already released."))
  (:documentation
   "An operation was attempted on an image that has already been destroyed.

Reaching this is much better than the alternative. The previous version handed
callers a bare foreign pointer, so the same mistake read freed memory.")
  (:report (lambda (c stream)
             (declare (ignore c))
             (format stream "Operation on an image that has already been destroyed."))))

;;; Library loading -----------------------------------------------------------

(define-condition library-not-found (im-error)
  ((candidates :initarg :candidates :initform nil
               :reader library-not-found-candidates
               :documentation "The names and paths actually tried, in order."))
  (:documentation
   "An IM shared library could not be opened.")
  (:report (lambda (c stream)
             (format stream "Cannot load the IM library~@[ (~A)~].~
                             ~@[~%Tried:~{~%  ~A~}~]~
                             ~%Set IM_LIBRARY_PATH to the directory holding it."
                     (error-detail c)
                     (library-not-found-candidates c)))))

(define-condition display-unavailable (im-error) ()
  (:documentation
   "IM:DISPLAY found nothing able to show an image.

The PNG was written -- ERROR-DETAIL names it -- but no front end was listening:
no SLY or SLIME connection in this thread, and no IM:*DISPLAY-FUNCTION*. An
error rather than a quiet return, because the alternative is a caller who
believes an image is on screen somewhere.")
  (:report (lambda (c stream)
             (format stream "No REPL front end to display an image in~
                             ~@[; it was written to ~A~].~%~
                             SLY needs sly-enable-evaluate-in-emacs set to t; ~
                             SLIME needs the slime-media contrib.~%~
                             Bind IM:*DISPLAY-FUNCTION* to display it another way."
                     (error-detail c)))))

;;; Capture -------------------------------------------------------------------
;;;
;;; These were the five conditions that did not inherit from CL:ERROR. They do
;;; now, and they are subtypes of one CAPTURE-ERROR so a caller can handle all
;;; device trouble at once.

(define-condition capture-error (im-error) ()
  (:documentation "A video capture operation failed."))

(define-condition no-device-error (capture-error) ()
  (:documentation "No capture device is available.

Normal on Linux, where upstream builds a stub backend that reports zero
devices so the symbol set stays identical across platforms.")
  (:report (lambda (c stream)
             (declare (ignore c))
             (format stream "No video capture devices are available."))))

(define-condition invalid-device-error (capture-error) ()
  (:documentation "The requested capture device index does not exist."))

(define-condition device-connection-error (capture-error) ()
  (:documentation "Connecting to or disconnecting from a capture device failed.

On macOS a process without NSCameraUsageDescription is killed by TCC rather
than being allowed to fail, so this covers connection refusals that are
actually reportable."))

(define-condition device-configuration-error (capture-error) ()
  (:documentation "Setting a capture format, size or attribute failed."))
