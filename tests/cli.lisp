;;;; tests/cli.lisp — the built command-line tool.
;;;;
;;;; These run the binary as a subprocess rather than calling the handlers,
;;;; because the thing most worth testing about bin/im is exactly what
;;;; in-process tests cannot see: a dumped SBCL image comes back with no
;;;; foreign libraries open and with CFFI's bookkeeping claiming otherwise, so
;;;; a binding that works perfectly from source can still produce a binary
;;;; that cannot find libim -- or worse, silently binds to a different one.

(in-package #:im.tests)

(def-suite cli-suite :in im-suite
  :description "The bin/im executable, run as a subprocess.")
(in-suite cli-suite)

(defparameter *binary*
  (asdf:system-relative-pathname "im" "bin/im")
  "Built by `make'. These tests skip when it is absent.")

(defun binary-available-p ()
  (and (probe-file *binary*) t))

(defun run-cli (&rest arguments)
  "Run bin/im with ARGUMENTS. Returns (values stdout stderr exit-code)."
  (uiop:run-program (cons (namestring *binary*) arguments)
                    :output :string
                    :error-output :string
                    :ignore-error-status t))

(defmacro with-cli (&body body)
  "Run BODY only when the binary has been built."
  `(if (binary-available-p)
       (progn ,@body)
       (skip "bin/im is not built; run `make' first")))

(test binary-runs-standalone
  "The dumped image reopens libim on restore.

If the image-restore hook were missing, CFFI would believe the library was
already open -- that belief survives SAVE-LISP-AND-DIE -- and never call
dlopen, leaving the process bound to whatever the loader happened to provide."
  (with-cli
    (multiple-value-bind (out err code) (run-cli "library")
      (declare (ignore err))
      (is (zerop code))
      (is (search "im version" out))
      (is (search "libim" out) "the report must name the library it opened"))))

(test info-matches-the-library
  (with-cli
    (multiple-value-bind (out err code)
        (run-cli "info" (namestring (image-file "lena.jpg")))
      (declare (ignore err))
      (is (zerop code))
      (is (search "JPEG" out)))))

(test json-output-is-valid-json
  "--json must produce one parseable value, with strings as strings.

A string is a vector of characters, so a serialiser that checks for vectors
before strings turns \"JPEG\" into [\"J\",\"P\",\"E\",\"G\"]."
  (with-cli
    (multiple-value-bind (out err code)
        (run-cli "info" (namestring (image-file "lena.jpg")) "--json")
      (declare (ignore err))
      (is (zerop code))
      (let ((parsed (shasht:read-json out)))
        (is (typep parsed 'hash-table))
        (is (equal "JPEG" (gethash "format" parsed)))))))

(test exit-codes-distinguish-failure-kinds
  (with-cli
    (is (zerop (nth-value 2 (run-cli "info" (namestring (image-file "lena.jpg")))))
        "success exits 0")
    (is (= 1 (nth-value 2 (run-cli "info" "/nonexistent/missing.png")))
        "an IM error exits 1")
    (is (= 2 (nth-value 2 (run-cli "process" "a" "b")))
        "a usage error exits 2")))

(test errors-go-to-stderr-not-stdout
  "So that `im info --json x | jq' is not corrupted by a diagnostic."
  (with-cli
    (multiple-value-bind (out err code) (run-cli "info" "/nonexistent/missing.png")
      (is (= 1 code))
      (is (zerop (length (string-trim '(#\Space #\Newline) out)))
          "nothing on stdout")
      (is (search "im:" err) "the message is on stderr"))))

(test process-pipeline-applies-operations-in-order
  (with-cli
    (let ((output (namestring (tmp-file "cli-pipeline.png"))))
      (multiple-value-bind (out err code)
          (run-cli "process" (namestring (image-file "lena.jpg")) output
                   "--op" "resize=50%" "--op" "colorspace=gray" "--op" "sobel")
        (declare (ignore err))
        (is (zerop code))
        (is (search "color-space-gray" out))
        (is (probe-file output))
        ;; lena.jpg is 208x222, so half is 104x111.
        (im:with-image (result (im:load output))
          (is (= 104 (im:width result)))
          (is (= 111 (im:height result)))
          (is (eq :color-space-gray (im:color-space result))))))))

(test unknown-operation-is-reported-not-ignored
  (with-cli
    (multiple-value-bind (out err code)
        (run-cli "process" (namestring (image-file "lena.jpg"))
                 (namestring (tmp-file "unused.png")) "--op" "nosuchop")
      (declare (ignore out))
      (is (= 2 code))
      (is (search "nosuchop" err)))))

;;; Library-path handling -----------------------------------------------------

(def-suite cli-library-suite :in im-suite
  :description "How the CLI resolves --im-library, in process.")
(in-suite cli-library-suite)

(test already-loaded-from-p-tolerates-a-bare-soname
  "CFFI records what it was asked to open, not always a path.

A library found by search is recorded under its bare soname on Linux --
\"libim.so\" -- and TRUENAME on that signals rather than returning NIL. This
check runs before any subcommand does its work, so signalling here killed
every invocation of the binary on Linux with a file error naming a library in
the current directory. Answering NIL is fine; raising is not."
  (let ((im::*loaded* (make-hash-table :test #'eq)))
    (setf (gethash 'im::lib-im im::*loaded*) "libim.so")
    (finishes (im.cli::already-loaded-from-p "/tmp"))
    (is (null (im.cli::already-loaded-from-p "/tmp"))))
  ;; A directory that does not exist must not signal either.
  (finishes (im.cli::already-loaded-from-p "/nonexistent/directory")))

(test already-loaded-from-p-recognises-a-real-match
  (let ((path (im:library-pathname 'im::lib-im)))
    (when (and path (uiop:truename* path))
      (let ((dir (uiop:pathname-directory-pathname (uiop:truename* path))))
        (is-true (im.cli::already-loaded-from-p (namestring dir)))))))
