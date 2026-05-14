(in-package #:im-tests)

(def-suite render-suite :in im-suite
  :description "im-render package: synthetic image generation.")

(in-suite render-suite)

(test constant-fills-with-given-value
  (with-image (img (im-image:create 8 8 :color-space-gray :data-type-byte))
    (im-render:constant img 77)
    (loop for i below 64 do
      (is (= 77 (cffi:mem-aref (im-image:data img 0) :unsigned-char i))))))

(test random-noise-produces-non-uniform-output
  (with-image (img (im-image:create 32 32 :color-space-gray :data-type-byte))
    (im-render:random-noise img)
    ;; Output should have at least 4 distinct byte values.
    (let ((seen (make-array 256 :initial-element nil))
          (p (im-image:data img 0)))
      (loop for i below (* 32 32) do
        (setf (aref seen (cffi:mem-aref p :unsigned-char i)) t))
      (is (> (count t seen) 4)))))

(test add-gaussian-noise-increases-variance
  (with-images ((src (im-image:create 64 64 :color-space-gray :data-type-byte))
                (dst (im-image:create 64 64 :color-space-gray :data-type-byte)))
    (im-render:constant src 100)
    (im-render:add-gaussian-noise src dst 0 15)
    ;; Output should not be constant
    (let ((seen (make-array 256 :initial-element nil))
          (p (im-image:data dst 0)))
      (loop for i below (* 64 64) do
        (setf (aref seen (cffi:mem-aref p :unsigned-char i)) t))
      (is (> (count t seen) 1)))))

(test add-uniform-noise-changes-pixels
  (with-images ((src (im-image:create 64 64 :color-space-gray :data-type-byte))
                (dst (im-image:create 64 64 :color-space-gray :data-type-byte)))
    (im-render:constant src 100)
    (im-render:add-uniform-noise src dst 0 20)
    (let ((diff 0)
          (sp (im-image:data src 0))
          (dp (im-image:data dst 0)))
      (loop for i below (* 64 64) do
        (when (/= (cffi:mem-aref sp :unsigned-char i)
                  (cffi:mem-aref dp :unsigned-char i))
          (incf diff)))
      (is (> diff 0)))))

(test add-speckle-noise-changes-pixels
  (with-images ((src (im-image:create 64 64 :color-space-gray :data-type-byte))
                (dst (im-image:create 64 64 :color-space-gray :data-type-byte)))
    (im-render:constant src 100)
    (im-render:add-speckle-noise src dst 10)
    (let ((diff 0)
          (sp (im-image:data src 0))
          (dp (im-image:data dst 0)))
      (loop for i below (* 64 64) do
        (when (/= (cffi:mem-aref sp :unsigned-char i)
                  (cffi:mem-aref dp :unsigned-char i))
          (incf diff)))
      (is (> diff 0)))))

(test chessboard-and-grid-render-without-error
  (with-image (img (im-image:create 16 16 :color-space-gray :data-type-byte))
    (im-render:chessboard img 4 4)
    (im-render:grid img 4 4)
    ;; just confirm we got non-uniform output
    (let ((seen (make-array 256 :initial-element nil))
          (p (im-image:data img 0)))
      (loop for i below (* 16 16) do
        (setf (aref seen (cffi:mem-aref p :unsigned-char i)) t))
      (is (> (count t seen) 1)))))

(test gaussian-render-produces-bell-shape
  (with-image (img (im-image:create 32 32 :color-space-gray :data-type-byte))
    (im-render:gaussian img 5.0)
    ;; The center pixel should be brighter than corner pixels
    (is (> (pixel-byte img 0 16 16) (pixel-byte img 0 0 0)))))
