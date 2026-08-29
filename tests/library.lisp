;;;; tests/library.lisp — library discovery, version, and the binding manifest.

(in-package #:im.tests)

(def-suite library-suite :in im-suite
  :description "Loading the IM shared libraries and what they report.")
(in-suite library-suite)

(test core-library-is-loaded
  (is-true (im:library-loaded-p 'im::lib-im))
  (is (stringp (im:library-pathname 'im::lib-im))))

(test version-is-reported
  (is (stringp (im:version)))
  (is (plusp (length (im:version))))
  (is (integerp (im:version-number)))
  ;; IM 3.15 is 315000 plus the bugfix number. Anything below 3.x means the
  ;; wrong library was found entirely.
  (is (>= (im:version-number) 300000)))

(test every-binding-resolves
  "Every C function this binding declares exists in the loaded libraries.

This is the check the previous binding lacked, and it is the reason four dead
bindings survived in it for years: imCompressDataLZO and imCompressDataUnLZO
had been replaced upstream by LZ4, and imFormatRegisterAVI, imFormatRegisterWMV
and imFormatRegisterECW were declared in headers that no library implemented.
Each looked fine until it was called."
  (let ((missing '()))
    (dolist (entry im.ffi::*bindings*)
      (destructuring-bind (c-name . library) entry
        ;; Add-ons are legitimately absent -- IM_BUILD_HEIF and IM_BUILD_CAPTURE
        ;; default to OFF upstream, and the Linux CI builds without JP2. Only
        ;; check a library that actually loaded.
        (let ((designator (find-symbol (string-upcase library) :im)))
          (when (and designator (im:library-loaded-p designator))
            (unless (cffi:foreign-symbol-pointer c-name)
              (push c-name missing))))))
    (is (null missing)
        "~D bound function~:P do not exist in the loaded IM: ~{~A~^, ~}"
        (length missing) missing)))

(test process-libraries-are-mutually-exclusive
  "Only one of im_process and im_process_omp may be open at a time.

They export identical symbol sets, so with both loaded which implementation a
call reaches is decided by dlsym search order rather than by this code."
  (is (not (and (im:library-loaded-p 'im::lib-im-process)
                (im:library-loaded-p 'im::lib-im-process-omp)))))

(test missing-library-signals-library-not-found
  "A library that cannot be opened reports what was tried, not a CLOS error."
  (cffi:define-foreign-library probe-missing-library
    (t (:default "im_tests_definitely_absent")))
  (signals im:library-not-found (im::%load-one 'probe-missing-library))
  (handler-case (im::%load-one 'probe-missing-library)
    (im:library-not-found (c)
      (is (listp (im:library-not-found-candidates c)))
      (is (plusp (length (im:library-not-found-candidates c)))))))
