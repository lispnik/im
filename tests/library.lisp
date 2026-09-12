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

(defun %binding-library (name)
  "The designator to check a binding against, given the manifest's NAME for it.

The manifest records every im_process binding as LIB-IM-PROCESS, but the
loader's default :AUTO preference opens LIB-IM-PROCESS-OMP instead, and
PROCESS-LIBRARIES-ARE-MUTUALLY-EXCLUSIVE below guarantees only one of them is
ever open. Taking the manifest name literally therefore skipped every one of
those bindings -- 206 of 459, all of libim_process among them -- so the check
below quietly covered a little over half of what its docstring claims, on
every platform, because the release ships both libraries and :AUTO always
picks the OpenMP one.

The two export identical symbol sets, which is what makes either acceptable
here and is the same fact FFT-SYMBOL has to work around."
  (let* ((named (find-symbol (string-upcase name) :im))
         (omp (find-symbol "LIB-IM-PROCESS-OMP" :im)))
    (if (and named omp
             (eq named (find-symbol "LIB-IM-PROCESS" :im))
             (not (im:library-loaded-p named))
             (im:library-loaded-p omp))
        omp
        named)))

(test every-binding-resolves
  "Every C function this binding declares exists in the loaded libraries.

This is the check the previous binding lacked, and it is the reason four dead
bindings survived in it for years: imCompressDataLZO and imCompressDataUnLZO
had been replaced upstream by LZ4, and imFormatRegisterAVI, imFormatRegisterWMV
and imFormatRegisterECW were declared in headers that no library implemented.
Each looked fine until it was called."
  (let ((missing '())
        (skipped 0))
    (dolist (entry im.ffi::*bindings*)
      (destructuring-bind (c-name . library) entry
        ;; Add-ons are legitimately absent -- IM_BUILD_HEIF and IM_BUILD_CAPTURE
        ;; default to OFF upstream, and the Linux CI builds without JP2. Only
        ;; check a library that actually loaded.
        (let ((designator (%binding-library library)))
          (if (and designator (im:library-loaded-p designator))
              (unless (cffi:foreign-symbol-pointer c-name)
                (push c-name missing))
              (incf skipped)))))
    (is (null missing)
        "~D bound function~:P do not exist in the loaded IM: ~{~A~^, ~}"
        (length missing) missing)
    ;; Coverage is itself an assertion. This check reports success by finding
    ;; nothing, so a designator that stops matching turns it into a no-op that
    ;; still passes -- which is exactly what happened with LIB-IM-PROCESS. Only
    ;; the optional add-ons should ever be skipped.
    (is (< skipped (floor (length im.ffi::*bindings*) 4))
        "skipped ~D of ~D bindings; that is too many to call this check covered"
        skipped (length im.ffi::*bindings*))))

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
