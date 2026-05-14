(in-package #:im-tests)

(def-suite counter-suite :in im-suite
  :description "im-counter package: callback registration, scoped install, abort.")

(in-suite counter-suite)

(test rotate90-fires-begin-end-and-progress
  (let ((begin-count 0)
        (end-count 0)
        (other-progress 0))
    (im-counter:with-callback
        ((lambda (id text progress)
           (declare (ignore id text))
           (cond ((= progress -1)   (incf begin-count))
                 ((= progress 1001) (incf end-count))
                 (t                 (incf other-progress)))
           t))
      (with-images ((src (make-gray-gradient 64 48))
                    (dst (im-image:create 48 64 :color-space-gray :data-type-byte)))
        (im-geometric:rotate-90 src dst)))
    (is (>= begin-count 1) "begin event fired")
    (is (>= end-count 1)   "end event fired")
    (is (>= other-progress 1) "at least one progress increment")))

(test with-callback-restores-previous-on-exit
  (im-counter:with-callback ((lambda (i tx p) (declare (ignore i tx p)) t))
    ;; While the outer callback is active a counter exists.
    (is (im-counter:has-callback-p)))
  ;; After scope exit, no Lisp callback should be installed any more.
  ;; (The C side may still have iCounterUserData set as a stale
  ;; pointer; what we verify here is just the Lisp-visible state.)
  (is (not (im-counter:has-callback-p))))

(test abort-causes-process-op-to-fail-counter-aborted
  ;; Returning NIL from the callback aborts the operation. The
  ;; counter-aware op then signals IM-PROCESS:COUNTER-ABORTED.
  (signals im-process:counter-aborted
    (im-counter:with-callback
        ((lambda (id text progress)
           (declare (ignore id text progress))
           nil))                          ; abort immediately
      (with-images ((src (make-gray-gradient 64 48))
                    (dst (im-image:create 48 64 :color-space-gray :data-type-byte)))
        (im-geometric:rotate-90 src dst)))))
