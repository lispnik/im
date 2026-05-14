(in-package #:im-tests)

(def-suite threshold-suite :in im-suite
  :description "im-threshold package: threshold variants and helpers.")

(in-suite threshold-suite)

(test threshold-marks-pixels-above-level
  (with-images ((src (make-gray-gradient 64 1))
                (dst (im-image:create 64 1 :color-space-binary :data-type-byte)))
    (im-threshold:threshold src dst 32)
    ;; In our gradient (0..63), values > 32 become 1; values <= 32 are 0.
    (is (= 0 (pixel-byte dst 0 32 0)))    ; gray=32, equal threshold
    (is (= 1 (pixel-byte dst 0 33 0)))    ; gray=33 > 32
    (is (= 0 (pixel-byte dst 0 0 0)))))

(test otsu-returns-threshold-and-binarises-bimodal-data
  (with-images ((src (im-image:create 32 32 :color-space-gray :data-type-byte))
                (dst (im-image:create 32 32 :color-space-binary :data-type-byte)))
    (let ((p (im-image:data src 0)))
      (loop for i below (* 32 32) do
        (setf (cffi:mem-aref p :unsigned-char i)
              (if (oddp i) 50 200))))
    (let ((level (im-threshold:otsu src dst)))
      (is (< 50 level 200)
          "otsu threshold ~A should fall between the two modes" level))
    ;; about half of the pixels should be set
    (let ((set (count-set-pixels dst))
          (total (* 32 32)))
      (is (< (/ total 4) set (* 3 (/ total 4))))) ))

(test threshold-by-diff-is-strictly-greater
  (with-images ((a (im-image:create 16 16 :color-space-gray :data-type-byte))
                (b (im-image:create 16 16 :color-space-gray :data-type-byte))
                (d (im-image:create 16 16 :color-space-binary :data-type-byte)))
    (let ((pa (im-image:data a 0))
          (pb (im-image:data b 0)))
      (loop for i below (* 16 16) do
        (setf (cffi:mem-aref pa :unsigned-char i) 100
              (cffi:mem-aref pb :unsigned-char i) 100))
      ;; Make a 4x4 block where a > b
      (loop for y from 4 below 8 do
        (loop for x from 4 below 8 do
          (setf (cffi:mem-aref pa :unsigned-char (+ (* y 16) x)) 250))))
    (im-threshold:threshold-by-diff a b d)
    (is (= 16 (count-set-pixels d)))))

(test hysteresis-threshold-propagates-from-strong-to-weak
  (with-images ((src (im-image:create 16 16 :color-space-gray :data-type-byte))
                (dst (im-image:create 16 16 :color-space-binary :data-type-byte)))
    (let ((p (im-image:data src 0)))
      (loop for i below (* 16 16) do (setf (cffi:mem-aref p :unsigned-char i) 0))
      (setf (cffi:mem-aref p :unsigned-char (+ (* 7 16) 7)) 200) ; strong
      (setf (cffi:mem-aref p :unsigned-char (+ (* 8 16) 7)) 100) ; weak adjacent
      (setf (cffi:mem-aref p :unsigned-char (+ (* 12 16) 12)) 100)) ; weak isolated
    (im-threshold:hysteresis src dst 80 180)
    (is (= 1 (pixel-byte dst 0 7 7)))
    (is (= 1 (pixel-byte dst 0 7 8)))    ; weak attached -> set
    (is (= 0 (pixel-byte dst 0 12 12)))  ; weak isolated -> not set
    ))

(test hysteresis-estimate-returns-low-and-high
  (with-image (img (make-gray-gradient 32 32))
    (multiple-value-bind (low high)
        (im-threshold:hysteresis-estimate img)
      (is (>= low 0))
      (is (>= high low)))))

(test local-max-finds-isolated-peak
  (with-images ((src (im-image:create 32 32 :color-space-gray :data-type-byte))
                (dst (im-image:create 32 32 :color-space-binary :data-type-byte)))
    (let ((p (im-image:data src 0)))
      (loop for i below (* 32 32) do (setf (cffi:mem-aref p :unsigned-char i) 30))
      (setf (cffi:mem-aref p :unsigned-char (+ (* 16 32) 16)) 250))
    (im-threshold:local-max src dst 3 100)
    (is (= 1 (pixel-byte dst 0 16 16)))))

(test slice-threshold-keeps-band
  (with-images ((src (make-gray-gradient 64 1))
                (dst (im-image:create 64 1 :color-space-binary :data-type-byte)))
    (im-threshold:slice src dst 20 40)
    ;; Pixels with gray in [20, 40] should be 1; outside should be 0.
    (is (= 0 (pixel-byte dst 0 10 0)))
    (is (= 1 (pixel-byte dst 0 25 0)))
    (is (= 0 (pixel-byte dst 0 50 0)))))
