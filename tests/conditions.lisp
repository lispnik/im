;;;; tests/conditions.lisp — the condition hierarchy and the restart protocol.
;;;;
;;;; The previous suite had two SIGNALS assertions in 279 and never asserted on
;;;; IM-ERROR at all, which is how a COUNTER-ABORTED that silently returned NIL
;;;; and five capture conditions that did not inherit from CL:ERROR all
;;;; survived.

(in-package #:im.tests)

(def-suite condition-suite :in im-suite
  :description "Errors, their classes, and cancellation restarts.")
(in-suite condition-suite)

(test every-condition-is-an-error
  "Everything this library signals with ERROR must be a SERIOUS-CONDITION."
  (dolist (name '(im:im-error im:io-error im:open-error im:access-error
                  im:format-error im:data-error im:compress-error
                  im:memory-error im:invalid-image im:library-not-found
                  im:operation-aborted im:capture-error im:no-device-error
                  im:invalid-device-error im:device-connection-error
                  im:device-configuration-error))
    (is (subtypep name 'cl:error) "~A must be a subtype of CL:ERROR" name)
    (is (subtypep name 'im:im-condition) "~A must be an IM:IM-CONDITION" name)))

(test error-classes-are-dispatchable
  "A caller distinguishes causes by class, not by testing a slot."
  (signals im:open-error (im:load #p"/nonexistent/definitely-missing.png"))
  (handler-case (im:load #p"/nonexistent/definitely-missing.png")
    (im:open-error (c)
      (is (eq :error-code-open (im:error-code c)))
      (is-true (im:error-detail c) "the report must name the file"))))

(test unknown-extension-is-a-format-error
  (im:with-image (image (im:create 4 4 :color-space-gray :data-type-byte))
    (signals im:format-error (im:save image #p"/tmp/im-tests-unknown.zzz"))))

(test conditions-report-usefully
  "Every condition prints something more specific than its class name."
  (dolist (form (list (make-condition 'im:open-error :detail "/tmp/x.png")
                      (make-condition 'im:invalid-image)
                      (make-condition 'im:operation-aborted
                                      :operation "convolve" :progress 500)
                      (make-condition 'im:library-not-found
                                      :detail "libim" :candidates '("a" "b"))))
    (let ((text (princ-to-string form)))
      (is (plusp (length text)))
      (is (not (search "#<" text)) "~A printed as an unreadable object" (type-of form)))))

;;; Cancellation --------------------------------------------------------------

(defun always-cancel (id text progress)
  (declare (ignore id text progress))
  nil)

(test cancelling-signals-operation-aborted
  "A callback returning NIL must raise, not return NIL and carry on.

This is the defect that mattered most in the previous binding: cancellation
used SIGNAL on a non-error condition, so with no handler installed the call
returned NIL and the next line read a half-written destination image."
  (im:with-images ((source (gray-gradient 256 256))
                   (dest (im:create 256 256 :color-space-gray :data-type-byte)))
    (signals im:operation-aborted
      (im:with-progress (#'always-cancel)
        (im:convolve-gaussian source dest 6.0d0)))))

(test aborted-condition-carries-context
  (im:with-images ((source (gray-gradient 256 256))
                   (dest (im:create 256 256 :color-space-gray :data-type-byte)))
    (handler-case
        (im:with-progress (#'always-cancel)
          (im:convolve-gaussian source dest 6.0d0))
      (im:operation-aborted (c)
        (is (stringp (im:operation-aborted-operation c)))
        (is (search "convolve" (im:operation-aborted-operation c)))))))

(test continue-restart-abandons-the-operation
  (im:with-images ((source (gray-gradient 256 256))
                   (dest (im:create 256 256 :color-space-gray :data-type-byte)))
    (is (null
         (handler-bind ((im:operation-aborted
                          (lambda (c) (declare (ignore c))
                            (invoke-restart 'continue))))
           (im:with-progress (#'always-cancel)
             (im:convolve-gaussian source dest 6.0d0)))))))

(test retry-restart-runs-the-operation-again
  "RETRY re-runs; a handler that stops cancelling lets the retry succeed."
  (let ((attempts 0))
    (im:with-images ((source (gray-gradient 128 128))
                     (dest (im:create 128 128 :color-space-gray :data-type-byte)))
      (handler-bind ((im:operation-aborted
                       (lambda (c) (declare (ignore c))
                         (invoke-restart 'retry))))
        (im:with-progress ((lambda (id text progress)
                             (declare (ignore id text progress))
                             ;; Cancel the first attempt only.
                             (incf attempts)
                             (> attempts 1)))
          (im:convolve-gaussian source dest 3.0d0))))
    (is (> attempts 1) "the operation must have been attempted more than once")))

(test progress-callback-is-removed-afterwards
  "WITH-PROGRESS must detach on the way out, including on a non-local exit.

A callback pointer left installed into a Lisp image that is later dumped and
restored is a crash with no useful backtrace."
  (im:with-images ((source (gray-gradient 64 64))
                   (dest (im:create 64 64 :color-space-gray :data-type-byte)))
    (ignore-errors
     (im:with-progress (#'always-cancel)
       (im:convolve-gaussian source dest 2.0d0)))
    (is (zerop (im.ffi::%im-counter-has-callback)))))

(test operations-complete-without-a-callback
  (im:with-images ((source (gray-gradient 64 64))
                   (dest (im:create 64 64 :color-space-gray :data-type-byte)))
    (finishes (im:convolve-gaussian source dest 2.0d0))))
