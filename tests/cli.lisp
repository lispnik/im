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

(test spectrum-is-legible-not-black
  "The --op spectrum output must show structure, not be almost entirely black.

IM applies its logarithmic gamma after rescaling the magnitudes by their own
max, which is the DC term -- so the gamma has to be as large as the FFT's
dynamic range. A too-small value (the original -10) left all but the DC
neighbourhood at zero, which reads as a black PNG."
  (with-cli
    (let ((output (namestring (tmp-file "cli-spectrum.png"))))
      (multiple-value-bind (out err code)
          (run-cli "process" (namestring (image-file "lena.jpg")) output
                   "--op" "spectrum")
        (declare (ignore out err))
        (is (zerop code))
        (im:with-image (result (im:load output))
          (let* ((plane (im:plane-pointer result 0))
                 (count (im:pixel-count result))
                 (nonzero (loop for i below count
                                count (plusp (cffi:mem-aref plane :unsigned-char i)))))
            ;; -10 lit ~12% of pixels; -1000 lights ~97%. Half is a wide margin
            ;; that still fails hard if the gamma regresses to near-black.
            (is (> nonzero (floor count 2))
                "spectrum is mostly black: only ~D of ~D pixels are lit"
                nonzero count)))))))

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

;;; The bundled layout ---------------------------------------------------------

(in-suite cli-suite)

(defun im-library-directory ()
  "The directory the IM shared libraries were loaded from, or NIL.

Prefers IM_LIBRARY_PATH, because what CFFI reports can be a bare soname
rather than a path."
  (let ((env (uiop:getenv "IM_LIBRARY_PATH")))
    (or (when (and env (plusp (length env)))
          (uiop:truename* (uiop:ensure-directory-pathname env)))
        (let ((loaded (im:library-pathname 'im::lib-im)))
          (when loaded
            (let ((truename (uiop:truename* loaded)))
              (when truename (uiop:pathname-directory-pathname truename))))))))

(test bundled-layout-is-self-sufficient
  "A binary sitting beside its libraries runs with no environment help.

This is exactly what the Windows release ships, and it has to be flat rather
than bin/ + lib/: when Windows loads im.dll it resolves im.dll's own
dependencies against the directory of the running executable, not against the
directory im.dll came from.

Run with IM_LIBRARY_PATH cleared, which is the point -- with it set the binary
would find the build tree and this would prove nothing."
  (with-cli
    (let ((source (im-library-directory)))
      (if (null source)
          (skip "cannot locate the IM libraries to copy")
          (let* ((bundle (uiop:ensure-directory-pathname (tmp-file "bundle/")))
                 (libraries (append (directory (merge-pathnames "libim*.*" source))
                                    (directory (merge-pathnames "im*.dll" source)))))
            (if (null libraries)
                (skip "no IM libraries found beside ~A" source)
                (progn
                  (ensure-directories-exist bundle)
                  (let ((executable (merge-pathnames "im" bundle)))
                    (uiop:copy-file *binary* executable)
                    ;; The copy loses the mode bits.
                    (uiop:run-program (list "chmod" "+x" (namestring executable))
                                      :ignore-error-status t)
                    (dolist (library libraries)
                      (uiop:copy-file library (merge-pathnames (file-namestring library)
                                                               bundle)))
                    (multiple-value-bind (out err code)
                        (uiop:run-program (list (namestring executable) "library")
                                          :output :string :error-output :string
                                          :ignore-error-status t
                                          ;; Clear the variable for the child only.
                                          :environment
                                          (remove-if (lambda (entry)
                                                       (uiop:string-prefix-p
                                                        "IM_LIBRARY_PATH=" entry))
                                                     (sb-ext:posix-environ)))
                      (declare (ignore err))
                      (is (zerop code)
                          "a binary beside its libraries must run unaided")
                      (is (search "im version" out)))))))))))

;;; im diff -------------------------------------------------------------------

(test diff-reports-identical-similar-and-different
  "The structural verdict tracks the actual relationship between the images."
  (with-cli
    ;; identical: the same file against itself
    (multiple-value-bind (out err code)
        (run-cli "diff" (namestring (image-file "lena.jpg"))
                 (namestring (image-file "lena.jpg")))
      (declare (ignore err))
      (is (zerop code))
      (is (search "identical" out))
      (is (search "rms error         0" out) "identical images have zero RMSE")
      (is (search "ssim              1" out) "identical images have SSIM 1"))
    ;; different images, and different sizes: perceptual hashes still answer,
    ;; the pixel metrics are skipped.
    (multiple-value-bind (out err code)
        (run-cli "diff" (namestring (image-file "lena.jpg"))
                 (namestring (image-file "flower.jpg")))
      (declare (ignore err))
      (is (zerop code))
      (is (search "dimensions match  -" out) "mismatched sizes are reported, not fatal")
      (is (search "different" out)))))

(test diff-writes-a-heatmap-for-same-size-images
  (with-cli
    (let ((blurred (namestring (tmp-file "diff-blur.png")))
          (heat (namestring (tmp-file "diff-heat.png")))
          (source (namestring (image-file "lena.jpg"))))
      ;; make a same-size variant that differs
      (run-cli "process" source blurred "--op" "gaussian=2")
      (multiple-value-bind (out err code)
          (run-cli "diff" source blurred "--output" heat)
        (declare (ignore err))
        (is (zerop code))
        (is (probe-file heat) "the heatmap was written")
        (is (search "similar" out))
        (im:with-image (h (im:load (pathname heat)))
          (is (eq :color-space-gray (im:color-space h))
              "the heatmap is a single-channel difference"))))))

;;; im montage ----------------------------------------------------------------

(test montage-composes-a-grid-from-mixed-images
  "A folder of mixed sizes and colour spaces becomes one RGB sheet."
  (with-cli
    (let ((sheet (namestring (tmp-file "montage-sheet.png"))))
      (multiple-value-bind (out err code)
          (run-cli "montage"
                   (namestring (image-file "lena.jpg"))     ; rgb
                   (namestring (image-file "rice.png"))     ; gray
                   (namestring (image-file "flower.jpg"))   ; different size
                   "--output" sheet "--columns" "2" "--tile" "100x100" "--gap" "10")
        (declare (ignore err))
        (is (zerop code))
        (is (probe-file sheet))
        (im:with-image (image (im:load (pathname sheet)))
          (is (eq :color-space-rgb (im:color-space image)))
          ;; 2 columns, 3 images -> 2 rows; each cell 100 + 10 gap, plus a
          ;; leading gap: 10 + 2*(100+10) = 230 wide, 10 + 2*(100+10) = 230 tall.
          (is (= 230 (im:width image)))
          (is (= 230 (im:height image))))))))

(test montage-does-not-flip-the-tiles
  "PASTE addresses both images bottom-up; a sign slip there mirrors every tile.

A marker whose first linear half is white and second half black must come back
the same way round after a single-tile montage sized exactly to the image."
  (with-cli
    (let ((marker (tmp-file "montage-marker.png"))
          (sheet (namestring (tmp-file "montage-flip.png"))))
      (im:with-image (m (im:create 40 40 :color-space-gray :data-type-byte))
        (let ((p (im:plane-pointer m 0)) (n (im:pixel-count m)))
          (dotimes (i n) (setf (cffi:mem-aref p :unsigned-char i)
                               (if (< i (floor n 2)) 255 0))))
        (im:save m marker))
      ;; one tile the marker's own size, no gap: the sheet is the marker back.
      (run-cli "montage" (namestring marker) "--output" sheet
               "--columns" "1" "--tile" "40x40" "--gap" "0")
      (im:with-image (out (im:load (pathname sheet)))
        (let ((p (im:plane-pointer out 0)) (n (im:pixel-count out)))
          (is (> (cffi:mem-aref p :unsigned-char 0) 200)
              "first pixel stayed bright -- tile not mirrored")
          (is (< (cffi:mem-aref p :unsigned-char (1- n)) 55)
              "last pixel stayed dark -- tile not mirrored"))))))
