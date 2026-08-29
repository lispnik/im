;;;; src/counter.lisp — version reporting, progress and cancellation.
;;;;
;;;; This is where restarts genuinely belong. A convolution over a large image
;;;; takes long enough that a user may want to stop it, and IM already has the
;;;; protocol: a counter callback returning zero aborts the operation in
;;;; progress and the operation reports IM_ERR_COUNTER.
;;;;
;;;; The previous binding wired that up in a way that made cancellation
;;;; invisible. 111 defcfuns shared a :WRAPPER return type whose :FROM-C
;;;; translator called SIGNAL -- not ERROR -- on a bare CONDITION. With no
;;;; handler installed SIGNAL returns, so a cancelled operation returned NIL
;;;; and execution carried straight on into code that would read a
;;;; half-written destination image. The same translator returned NIL on the
;;;; success path too, so the C boolean was discarded either way and no caller
;;;; could tell the two apart.

(in-package #:im)

(export '(version
          version-number
          version-date
          with-progress
          *progress-callback*
          call-with-cancellation-restarts
          check-operation))

;;; Version -------------------------------------------------------------------

(defun version ()
  "The IM library version as a string, e.g. \"3.15\"."
  (im.ffi::%im-version))

(defun version-date ()
  "The IM library's release date as a string."
  (im.ffi::%im-version-date))

(defun version-number ()
  "The IM library version as an integer, e.g. 315000.

IM adds the bugfix number to the compiled-in IM_VERSION_NUMBER, so comparing
this against the value a header advertised detects a library that has drifted
from the headers a binding was generated against."
  (im.ffi::%im-version-number))

;;; Progress and cancellation -------------------------------------------------

(defvar *progress-callback* nil
  "A function of (COUNTER TEXT PROGRESS) called as an IM operation proceeds,
or NIL.

PROGRESS is -1 at the start, 1001 at the end, and 0..1000 in between. TEXT is
the operation's title at the start and NIL later. Return true to continue,
NIL to cancel.

Bound by WITH-PROGRESS rather than set directly, so the previous callback is
always restored.")

(defvar *last-progress* nil
  "The most recent progress value seen, so a cancellation can report where.")

(defvar *cancelled-p* nil
  "True once a callback has asked to stop, for the extent of one operation.")

(cffi:defcallback %counter-trampoline :int
    ((counter :int) (user-data :pointer) (text :pointer) (progress :int))
  (declare (ignore user-data))
  (let ((callback *progress-callback*))
    (setf *last-progress* progress)
    (if (null callback)
        1
        ;; A Lisp condition must not unwind through C. IM is in the middle of
        ;; an operation with its own allocations, and a non-local exit here
        ;; would leave them. Catch everything, record the intent to stop, and
        ;; let the operation unwind on IM's own terms; the wrapper turns the
        ;; resulting IM_ERR_COUNTER into a Lisp error afterwards.
        (handler-case
            (let ((message (unless (cffi:null-pointer-p text)
                             (cffi:foreign-string-to-lisp text))))
              (if (funcall callback counter message progress)
                  1
                  (progn (setf *cancelled-p* t) 0)))
          (cl:error ()
            (setf *cancelled-p* t)
            0)))))

(defmacro with-progress ((callback) &body body)
  "Run BODY with CALLBACK receiving progress reports from IM operations.

CALLBACK is called as (CALLBACK COUNTER TEXT PROGRESS) and returns true to
continue, NIL to cancel. Cancelling makes the operation signal
OPERATION-ABORTED once IM has unwound.

  (im:with-progress ((lambda (id text pct)
                       (declare (ignore id text))
                       (< pct 500)))          ; stop halfway
    (im:convolve-sobel src dst))

IM keeps one global callback, so this installs and restores it around BODY
rather than adding to a list. Nesting works; concurrent use from two threads
does not, and cannot -- the limitation is IM's."
  (alexandria:with-gensyms (previous)
    `(let ((*progress-callback* ,callback)
           (*last-progress* nil)
           (*cancelled-p* nil)
           (,previous (im.ffi::%im-counter-has-callback)))
       (declare (ignorable ,previous))
       (unwind-protect
            (progn
              (im.ffi::%im-counter-set-callback (cffi:null-pointer)
                                                (cffi:callback %counter-trampoline))
              ,@body)
         ;; Always detach. A dangling callback pointer into a Lisp image that
         ;; has since been dumped and restored is a crash with no useful
         ;; backtrace.
         (im.ffi::%im-counter-set-callback (cffi:null-pointer)
                                           (cffi:null-pointer))))))

;;; The restart protocol ------------------------------------------------------

(defun call-with-cancellation-restarts (name thunk)
  "Call THUNK, offering RETRY and CONTINUE if it reports cancellation.

THUNK returns true on success and NIL when IM stopped early. Most IM
processing functions signal exactly that way -- an int that is zero when the
counter aborted them.

RETRY is worth having because the handle is still good: the destination image
was partly written, but nothing is corrupt and running again is well defined.
CONTINUE abandons the operation and returns NIL, for a caller who asked to
cancel and meant it."
  (loop
    (let ((outcome
            (restart-case
                (let ((result (funcall thunk)))
                  (if result
                      (return result)
                      (cl:error 'operation-aborted
                                :operation name
                                :progress *last-progress*)))
              (retry ()
                :report (lambda (s) (format s "Run ~A again." name))
                :retry)
              (continue ()
                :report (lambda (s) (format s "Abandon ~A and return NIL." name))
                nil))))
      (unless (eq outcome :retry)
        (return nil)))))

(defmacro check-operation (name &body call)
  "Wrap an IM processing call in the cancellation restarts.

CALL must evaluate to true on success and NIL when the counter aborted it,
which is what IM's int-returning process functions do."
  `(call-with-cancellation-restarts ,name (lambda () ,@call)))
