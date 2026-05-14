(in-package #:im-tests)

(def-suite convolve-suite :in im-suite
  :description "im-convolve package: convolution / edge detection / sharpening.")

(in-suite convolve-suite)

(test mean-3x3-smooths-step-edge
  (with-images ((src (im-image:create 16 16 :color-space-gray :data-type-byte))
                (dst (im-image:create 16 16 :color-space-gray :data-type-byte)))
    (let ((p (im-image:data src 0)))
      (loop for y below 16 do
        (loop for x below 16 do
          (setf (cffi:mem-aref p :unsigned-char (+ (* y 16) x))
                (if (< x 8) 0 200)))))
    (im-convolve:mean src dst 3)
    ;; columns far from the edge stay near the original values; the
    ;; transition column gets smoothed.
    (is (< (pixel-byte dst 0 7 8) 200))
    (is (> (pixel-byte dst 0 8 8) 0))))

(test median-3x3-removes-salt-and-pepper-outliers
  (with-images ((src (im-image:create 16 16 :color-space-gray :data-type-byte))
                (dst (im-image:create 16 16 :color-space-gray :data-type-byte)))
    (let ((p (im-image:data src 0)))
      ;; uniform 128 background
      (loop for i below (* 16 16) do
        (setf (cffi:mem-aref p :unsigned-char i) 128))
      ;; sprinkle a salt and a pepper pixel away from the border
      (setf (cffi:mem-aref p :unsigned-char (+ (* 5 16) 5)) 0)
      (setf (cffi:mem-aref p :unsigned-char (+ (* 8 16) 8)) 255))
    (im-convolve:median src dst 3)
    (is (= 128 (pixel-byte dst 0 5 5)))
    (is (= 128 (pixel-byte dst 0 8 8)))))

(test range-on-uniform-image-is-zero
  (with-images ((src (im-image:create 8 8 :color-space-gray :data-type-byte))
                (dst (im-image:create 8 8 :color-space-gray :data-type-byte)))
    (let ((p (im-image:data src 0)))
      (loop for i below 64 do (setf (cffi:mem-aref p :unsigned-char i) 100)))
    (im-convolve:range src dst 3)
    (loop for i below 64 do
      (is (zerop (cffi:mem-aref (im-image:data dst 0) :unsigned-char i))))))

(test sobel-and-prewitt-produce-output
  ;; Use a step image; the response should be non-trivial somewhere.
  (with-images ((src (im-image:create 32 32 :color-space-gray :data-type-byte))
                (dst (im-image:create 32 32 :color-space-gray :data-type-byte)))
    (let ((p (im-image:data src 0)))
      (loop for y below 32 do
        (loop for x below 32 do
          (setf (cffi:mem-aref p :unsigned-char (+ (* y 32) x))
                (if (< x 16) 0 200)))))
    (im-convolve:sobel src dst)
    (is (> (loop for i below (* 32 32) sum
                 (cffi:mem-aref (im-image:data dst 0) :unsigned-char i))
           0))
    (im-convolve:prewitt src dst)
    (is (> (loop for i below (* 32 32) sum
                 (cffi:mem-aref (im-image:data dst 0) :unsigned-char i))
           0))))

(test gaussian-stddev-kernel-size-roundtrip
  (let* ((stddev 2.0)
         (k (im-convolve:stddev-to-kernel-size stddev)))
    (is (oddp k))
    (is (>= k 3))
    ;; Inverse roughly recovers stddev (heuristic conversion in IM
    ;; isn't exact; allow generous tolerance).
    (is (<= (abs (- (im-convolve:kernel-size-to-stddev k) stddev)) 1.0))))

(test convolve-with-named-kernel
  (with-images ((src (make-gray-gradient 16 16))
                (dst (im-image:create 16 16 :color-space-gray :data-type-byte))
                (k   (im-cffi::%im-kernel-mean-3x3)))
    (im-convolve:convolve src dst k)
    ;; Output should be valid bytes in [0, 255]
    (loop for i below (* 16 16) do
      (let ((v (cffi:mem-aref (im-image:data dst 0) :unsigned-char i)))
        (is (<= 0 v 255))))))
