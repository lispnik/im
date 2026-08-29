;;;; src/ffi/im-counter.lisp — DRAFTED by tools/gen-bindings.lisp.
;;;;
;;;; Source: im_counter.h
;;;; Hand corrections below this line are expected and are kept;
;;;; re-run the generator into a clean tree and diff.

(in-package #:im.ffi)

(cffi:defcfun ("imCounterSetCallback" %im-counter-set-callback) :pointer
  "Changes the counter callback. Returns old callback. User data is changed
only if not NULL."
  (cb-user-data :pointer)
  (counter-func :pointer))

(cffi:defcfun ("imCounterHasCallback" %im-counter-has-callback) :int
  "Returns true if the counter callback is set. When the callback is NULL the
counter is inactive and all functions do nothing.")

(cffi:defcfun ("imCounterBegin" %im-counter-begin) :int
  "Begins a new count. Calls the callback with \"-1\" and text=title. This is
to be used by the operations. Returns a new counter Id. Several counters
can coexist at the same time, as part of a sequence with sub-counter or
simultaneous counter in multi-thread applications."
  (title :string))

(cffi:defcfun ("imCounterEnd" %im-counter-end) :void
  "Ends a count. Calls the callback with \"1001\", text=null, and releases
the counter."
  (counter :int))

(cffi:defcfun ("imCounterInc" %im-counter-inc) :int
  "Increments a count. Must set the total first. Calls the callback,
text=message if it is the first increment for the count. Returns 0 if the
callback aborted, 1 if returns normally."
  (counter :int))

(cffi:defcfun ("imCounterIncTo" %im-counter-inc-to) :int
  "Set a specific count. Must set the total first. Calls the callback,
text=message if it is the first increment for the count. Returns 0 if the
callback aborted, 1 if returns normally."
  (counter :int)
  (count :int))

(cffi:defcfun ("imCounterTotal" %im-counter-total) :void
  "Sets the total increments of a count. Must be set at least one time.
Notice that if total is set more than one time counter should simply
restart."
  (counter :int)
  (total :int)
  (message :string))

(cffi:defcfun ("imCounterGetUserData" %im-counter-get-user-data) :pointer
  "Sets an additional user data in the counter. Used to save the lock in
multi-threaded configurations."
  (counter :int))

(cffi:defcfun ("imCounterSetUserData" %im-counter-set-user-data) :void
  "Returns the additional user data in the counter."
  (counter :int)
  (userdata :pointer))
