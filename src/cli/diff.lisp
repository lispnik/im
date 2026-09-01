;;;; src/cli/diff.lisp — `im diff', structural and perceptual image comparison.
;;;;
;;;; `im compare' already answers "how far apart are these pixels" with RMSE
;;;; and SNR. `im diff' answers the questions a person actually asks of two
;;;; images: are they structurally the same (SSIM), are they perceptually the
;;;; same even at different sizes or encodings (perceptual hashes), and -- with
;;;; --output -- where do they differ (a heatmap).
;;;;
;;;; The perceptual hashes reduce each image to a 64-bit fingerprint, so they
;;;; compare images of different sizes; SSIM, PSNR and the heatmap need the two
;;;; to line up pixel for pixel and are reported only then.

(in-package #:im.cli)

(defun %byte-plane (image plane)
  "Plane PLANE of a byte IMAGE as a (VECTOR (UNSIGNED-BYTE 8)).

Cheaper to read once into Lisp than to reach across the FFI per pixel in the
loops below, and every caller here has already reduced the image to bytes."
  (let* ((count (im:pixel-count image))
         (pointer (im:plane-pointer image plane))
         (vector (make-array count :element-type '(unsigned-byte 8))))
    (dotimes (i count vector)
      (setf (aref vector i) (cffi:mem-aref pointer :unsigned-char i)))))

;;; Structural similarity -----------------------------------------------------

(defun %global-ssim (a b)
  "The global SSIM of two equal-length byte vectors, in [-1, 1].

Global rather than the usual 11x11 windowed mean: one statistic over the whole
image. It still captures the three terms SSIM is built from -- luminance,
contrast and structure -- and needs no window bookkeeping; the tradeoff is that
it cannot localise where structure diverges, which is what the heatmap is for.
The stabilising constants are the standard C1/C2 for an 8-bit range."
  (let* ((n (length a))
         (c1 (* 0.01d0 255 0.01d0 255))    ; (0.01 * L)^2
         (c2 (* 0.03d0 255 0.03d0 255))    ; (0.03 * L)^2
         (sum-a 0d0) (sum-b 0d0))
    (dotimes (i n)
      (incf sum-a (aref a i))
      (incf sum-b (aref b i)))
    (let ((mean-a (/ sum-a n)) (mean-b (/ sum-b n))
          (var-a 0d0) (var-b 0d0) (cov 0d0))
      (dotimes (i n)
        (let ((da (- (aref a i) mean-a))
              (db (- (aref b i) mean-b)))
          (incf var-a (* da da))
          (incf var-b (* db db))
          (incf cov (* da db))))
      ;; Sample variance/covariance (n-1) is conventional; n is fine for a
      ;; whole-image statistic and avoids a divide when n is 1.
      (setf var-a (/ var-a n) var-b (/ var-b n) cov (/ cov n))
      (/ (* (+ (* 2 mean-a mean-b) c1) (+ (* 2 cov) c2))
         (* (+ (* mean-a mean-a) (* mean-b mean-b) c1)
            (+ var-a var-b c2))))))

;;; Perceptual hashes ----------------------------------------------------------

(defun %average-hash (gray)
  "The 64-bit average hash (aHash) of a grey byte image.

Reduce to 8x8, then set the bit for each cell brighter than the mean. Two
images with the same broad tone and layout share most of these bits whatever
their original size or format."
  (im:with-image (small (im:resized gray :width 8 :height 8 :order 1))
    (let* ((cells (%byte-plane small 0))
           (mean (/ (reduce #'+ cells) 64.0))
           (bits 0))
      (dotimes (i 64 bits)
        (setf bits (logior (ash bits 1) (if (> (aref cells i) mean) 1 0)))))))

(defun %difference-hash (gray)
  "The 64-bit difference hash (dHash) of a grey byte image.

Reduce to 9x8 and set each bit from whether a cell is brighter than the one to
its left. dHash keys on gradients rather than absolute tone, so it is steadier
than aHash under brightness and contrast changes."
  (im:with-image (small (im:resized gray :width 9 :height 8 :order 1))
    (let ((cells (%byte-plane small 0))
          (bits 0))
      (dotimes (row 8 bits)
        (dotimes (col 8)
          (let ((left (aref cells (+ (* row 9) col)))
                (right (aref cells (+ (* row 9) col 1))))
            (setf bits (logior (ash bits 1) (if (> right left) 1 0)))))))))

(defun %hamming (a b)
  "The number of differing bits between two 64-bit hashes."
  (logcount (logxor a b)))

(defun %hash-hex (hash)
  (format nil "~16,'0X" hash))

;;; The difference heatmap -----------------------------------------------------

(defun write-heatmap (first-image second-image path amplify)
  "Write a grey image to PATH whose brightness is the per-pixel difference
between the two images, taken over whichever plane differs most and multiplied
by AMPLIFY (clamped to 255). Black is identical; bright is changed."
  (let* ((count (im:pixel-count first-image))
         (planes (im:depth first-image)))
    (im:with-image (heat (im:create (im:width first-image) (im:height first-image)
                                    :color-space-gray :data-type-byte))
      (let ((a (loop for p below planes collect (im:plane-pointer first-image p)))
            (b (loop for p below planes collect (im:plane-pointer second-image p)))
            (out (im:plane-pointer heat 0)))
        (dotimes (i count)
          (let ((worst 0))
            (loop for pa in a for pb in b
                  for d = (abs (- (cffi:mem-aref pa :unsigned-char i)
                                  (cffi:mem-aref pb :unsigned-char i)))
                  do (when (> d worst) (setf worst d)))
            (setf (cffi:mem-aref out :unsigned-char i)
                  (min 255 (round (* worst amplify)))))))
      (im:save heat (pathname path))
      (namestring (pathname path)))))

;;; Verdict --------------------------------------------------------------------

(defun %verdict (dimensions-match rms dhash-distance)
  "A one-word summary, from the strongest evidence available.

Pixel-identical when the sizes match and RMSE is zero. Otherwise the dHash
distance, which is defined whatever the sizes are, buckets the rest: a handful
of bits is a re-encode or a resize of the same picture, a dozen is a clear
relative, more is a different image."
  (cond ((and dimensions-match rms (zerop rms)) :identical)
        ((<= dhash-distance 2) :near-identical)
        ((<= dhash-distance 10) :similar)
        (t :different)))

;;; ----------------------------------------------------------------------------

(defun diff/options ()
  (list
   (clingon:make-option
    :string :long-name "output" :short-name #\o :key :output
    :description "Write a difference heatmap image here (same-size images only)")
   (clingon:make-option
    :integer :long-name "amplify" :key :amplify :initial-value 1
    :description "Multiply the heatmap difference by this before clamping")))

(defun diff/handler (command)
  (apply-global-options command)
  (let ((arguments (clingon:command-arguments command))
        (output (clingon:getopt command :output))
        (amplify (clingon:getopt command :amplify)))
    (unless (= 2 (length arguments))
      (usage-error "diff needs exactly two files. Try `im diff --help'."))
    (destructuring-bind (a b) arguments
      (im:with-images ((first-image (im:load (pathname a)))
                       (second-image (im:load (pathname b))))
        (let ((match (and (= (im:width first-image) (im:width second-image))
                          (= (im:height first-image) (im:height second-image)))))
          (when (and output (not match))
            (usage-error "cannot write a heatmap for ~Dx~D against ~Dx~D; ~
                          the images must be the same size"
                         (im:width first-image) (im:height first-image)
                         (im:width second-image) (im:height second-image)))
          ;; Perceptual hashes work at any size; SSIM/PSNR need a match.
          (im:with-images ((gray-a (im:grayscale first-image))
                           (gray-b (im:grayscale second-image)))
            (let* ((ahash-a (%average-hash gray-a))
                   (ahash-b (%average-hash gray-b))
                   (dhash-a (%difference-hash gray-a))
                   (dhash-b (%difference-hash gray-b))
                   (dhash-distance (%hamming dhash-a dhash-b))
                   (rms (when match (im:rms-error first-image second-image))))
              (emit
               (append
                (list :first (pathname a)
                      :second (pathname b)
                      :dimensions-match match)
                (when match
                  (list :width (im:width first-image)
                        :height (im:height first-image)
                        :rms-error rms
                        :psnr (if (zerop rms)
                                  nil
                                  (* 20d0 (log (/ 255d0 rms) 10d0)))
                        :ssim (%global-ssim (%byte-plane gray-a 0)
                                            (%byte-plane gray-b 0))))
                (list :average-hash (%hash-hex ahash-a)
                      :difference-hash (%hash-hex dhash-a)
                      :ahash-distance (%hamming ahash-a ahash-b)
                      :dhash-distance dhash-distance
                      :verdict (%verdict match rms dhash-distance))
                (when output
                  (verbose "~&Writing heatmap to ~A~%" output)
                  (list :heatmap (write-heatmap first-image second-image
                                                output amplify))))))))))))

(register-subcommand
 (clingon:make-command
  :name "diff"
  :description "Compare two images structurally: SSIM, perceptual hash, heatmap"
  :usage "[--output HEATMAP] [--amplify N] FIRST SECOND"
  :options (diff/options)
  :handler (guarded #'diff/handler)))
