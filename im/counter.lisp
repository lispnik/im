(defpackage #:im-counter
  (:use #:common-lisp)
  (:export #:set-callback
           #:has-callback-p
           #:begin
           #:end
           #:inc
           #:inc-to
           #:total
           #:get-user-data
           #:set-user-data
           #:with-callback))

(in-package #:im-counter)

;;; Storage for the user's lisp callback. The C API stores both a
;;; function pointer and a void* user_data; we only have one slot to
;;; play with from Lisp because cffi:get-callback returns a per-symbol
;;; pointer. Multi-callback registration would need cffi:defcallback
;;; with a unique symbol per registration; this implementation
;;; supports a single active callback (matching the C semantics:
;;; imCounterSetCallback overwrites whatever was there).

(defparameter *callback-fn* nil
  "The Lisp function the user installed via SET-CALLBACK, or NIL.")

(cffi:defcallback %trampoline :int ((counter :int)
                                    (cb-user-data :pointer)
                                    (text :pointer)
                                    (progress :int))
  (declare (ignore cb-user-data))
  (let ((fn *callback-fn*))
    (if fn
        (handler-case
            (let ((text-string (unless (cffi:null-pointer-p text)
                                 (cffi:foreign-string-to-lisp text))))
              (if (funcall fn counter text-string progress)
                  1
                  0))
          (error () 0))
        ;; No Lisp callback installed; tell IM to keep going.
        1)))

(defun set-callback (callback-fn)
  "Install CALLBACK-FN as the global progress callback. The function
will be invoked as (CALLBACK-FN COUNTER-ID TEXT-OR-NIL PROGRESS) and
should return non-NIL to continue, NIL to abort. Returns the previous
callback function (or NIL if none).

PROGRESS is -1 for begin, 1001 for end, otherwise a 0..1000 progress
value. TEXT is the title at begin, the message at the start of a
count, or NIL otherwise."
  (let ((prev *callback-fn*))
    (setf *callback-fn* callback-fn)
    (if callback-fn
        (im-cffi::%im-counter-set-callback (cffi:null-pointer)
                                           (cffi:callback %trampoline))
        (im-cffi::%im-counter-set-callback (cffi:null-pointer)
                                           (cffi:null-pointer)))
    prev))

(defun has-callback-p ()
  "Returns true if a counter callback is currently registered."
  (im-cffi::%im-counter-has-callback))

(defun begin (title)
  "Begin a new counter. Returns the new counter id. The callback is
invoked once with progress=-1 and TEXT=TITLE."
  (im-cffi::%im-counter-begin title))

(defun end (counter)
  "End COUNTER. Calls the callback with progress=1001."
  (im-cffi::%im-counter-end counter))

(defun inc (counter)
  "Increment COUNTER by one. Returns T if the callback wants the
operation to continue, NIL if it wants to abort. TOTAL must have
been called first."
  (= 1 (im-cffi::%im-counter-inc counter)))

(defun inc-to (counter count)
  "Set COUNTER's current count directly. Returns T to continue,
NIL to abort."
  (= 1 (im-cffi::%im-counter-inc-to counter count)))

(defun total (counter total &optional message)
  "Set the total expected increments for COUNTER and an optional
MESSAGE displayed at the start of the count. Calling this more than
once restarts the counter."
  (im-cffi::%im-counter-total counter total (or message "")))

(defun get-user-data (counter)
  "Return the user-data pointer associated with COUNTER (or NIL)."
  (let ((p (im-cffi::%im-counter-get-user-data counter)))
    (unless (cffi:null-pointer-p p) p)))

(defun set-user-data (counter user-data)
  "Attach USER-DATA (a foreign pointer) to COUNTER."
  (im-cffi::%im-counter-set-user-data counter user-data))

(defmacro with-callback ((callback-fn) &body body)
  "Install CALLBACK-FN for the dynamic extent of BODY, restoring the
previous callback on exit (including non-local exits)."
  (let ((prev (gensym "PREV")))
    `(let ((,prev (set-callback ,callback-fn)))
       (unwind-protect (progn ,@body)
         (set-callback ,prev)))))
