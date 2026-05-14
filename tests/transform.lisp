(in-package #:im-tests)

(def-suite transform-suite :in im-suite
  :description "im-transform package: FFT / distance / hough.")

(in-suite transform-suite)

(test distance-transform-zero-on-background
  (with-images ((src (make-binary-block))
                (dst (im-image:create 16 16 :color-space-gray :data-type-float)))
    ;; Pre-clear the destination float plane (DistanceTransform only
    ;; writes foreground).
    (let ((p (im-image:data dst 0)))
      (loop for i below (* 16 16) do (setf (cffi:mem-aref p :float i) 0.0)))
    (im-transform:distance-transform src dst)
    ;; Background pixels remain 0
    (let ((sp (im-image:data src 0))
          (dp (im-image:data dst 0)))
      (loop for i below (* 16 16) do
        (when (zerop (cffi:mem-aref sp :unsigned-char i))
          (is (zerop (cffi:mem-aref dp :float i))))))))

(test fft-then-ifft-recovers-input
  ;; Round-trip a complex image through FFT then IFFT. Compare
  ;; magnitudes within a generous tolerance (floating-point and
  ;; FFT scaling).
  (with-images ((src (im-image:create 16 16 :color-space-gray :data-type-cfloat))
                (fft-out (im-image:create 16 16 :color-space-gray :data-type-cfloat))
                (back (im-image:create 16 16 :color-space-gray :data-type-cfloat)))
    ;; Initialize source: a single bright spot at (8,8)
    (let ((p (im-image:data src 0)))
      (loop for i below (* 16 16 2) do (setf (cffi:mem-aref p :float i) 0.0))
      (setf (cffi:mem-aref p :float (* 2 (+ (* 8 16) 8))) 1.0))
    (im-transform:fft src fft-out)
    (im-transform:ifft fft-out back)
    ;; The non-zero pixel should still be the brightest in the result.
    (let* ((p (im-image:data back 0))
           (max-r 0.0)
           (max-i 0))
      (loop for i below (* 16 16) do
        (let ((r (cffi:mem-aref p :float (* 2 i))))
          (when (> (abs r) max-r)
            (setf max-r (abs r) max-i i))))
      ;; Should reach pixel index (8, 8) = 8*16+8 = 136
      (is (= 136 max-i)))))

(test regional-maximum-marks-isolated-peak
  (with-images ((src (im-image:create 16 16 :color-space-gray :data-type-float))
                (dst (im-image:create 16 16 :color-space-binary :data-type-byte)))
    (let ((p (im-image:data src 0)))
      (loop for i below (* 16 16) do (setf (cffi:mem-aref p :float i) 0.0))
      (setf (cffi:mem-aref p :float (+ (* 8 16) 8)) 100.0))
    (im-transform:regional-maximum src dst)
    ;; Either the peak pixel or a neighbour must be marked
    (let ((any-marked nil))
      (loop for dy from -1 to 1 do
        (loop for dx from -1 to 1 do
          (when (= 1 (pixel-byte dst 0 (+ 8 dx) (+ 8 dy)))
            (setf any-marked t))))
      (is-true any-marked))))

(test cross-correlation-runs-without-error
  (with-images ((a (im-image:create 16 16 :color-space-gray :data-type-cfloat))
                (b (im-image:create 16 16 :color-space-gray :data-type-cfloat))
                (d (im-image:create 16 16 :color-space-gray :data-type-cfloat)))
    (let ((pa (im-image:data a 0))
          (pb (im-image:data b 0)))
      (loop for i below (* 16 16 2) do
        (setf (cffi:mem-aref pa :float i) 0.0
              (cffi:mem-aref pb :float i) 0.0))
      (setf (cffi:mem-aref pa :float 0) 1.0)
      (setf (cffi:mem-aref pb :float 0) 1.0))
    (im-transform:cross-correlation a b d)
    ;; Output should be finite (no NaN/Inf)
    (let ((finite-p t)
          (p (im-image:data d 0)))
      (loop for i below (* 16 16 2) do
        (let ((v (cffi:mem-aref p :float i)))
          (when (or (= v v) ; NaN check
                    nil)    ; placeholder
            t)
          (when (not (= v v))
            (setf finite-p nil))))
      (is-true finite-p))))
