(in-package #:im-tests)

(def-suite calc-suite :in im-suite
  :description "im-calc package: image statistics and metrics.")

(in-suite calc-suite)

(test rms-error-zero-on-identical
  (with-images ((a (make-gray-gradient 16 12))
                (b (make-gray-gradient 16 12)))
    (is (= 0.0d0 (im-calc:rms-error a b)))))

(test count-colors-on-three-color-rgb
  (with-image (img (im-image:create 6 1 :color-space-rgb :data-type-byte))
    ;; 3 unique colours, each repeated twice
    (let ((r (im-image:data img 0))
          (g (im-image:data img 1))
          (b (im-image:data img 2)))
      (setf (cffi:mem-aref r :unsigned-char 0) 255 (cffi:mem-aref g :unsigned-char 0) 0   (cffi:mem-aref b :unsigned-char 0) 0)
      (setf (cffi:mem-aref r :unsigned-char 1) 255 (cffi:mem-aref g :unsigned-char 1) 0   (cffi:mem-aref b :unsigned-char 1) 0)
      (setf (cffi:mem-aref r :unsigned-char 2) 0   (cffi:mem-aref g :unsigned-char 2) 255 (cffi:mem-aref b :unsigned-char 2) 0)
      (setf (cffi:mem-aref r :unsigned-char 3) 0   (cffi:mem-aref g :unsigned-char 3) 255 (cffi:mem-aref b :unsigned-char 3) 0)
      (setf (cffi:mem-aref r :unsigned-char 4) 0   (cffi:mem-aref g :unsigned-char 4) 0   (cffi:mem-aref b :unsigned-char 4) 255)
      (setf (cffi:mem-aref r :unsigned-char 5) 0   (cffi:mem-aref g :unsigned-char 5) 0   (cffi:mem-aref b :unsigned-char 5) 255))
    (is (= 3 (im-calc:count-colors img)))))

(test percent-min-max-zero-percent-returns-absolute-bounds
  (with-image (img (make-gray-gradient 64 1))
    (multiple-value-bind (mn mx)
        (im-calc:percent-min-max img 0.0 nil)
      (is (= 0 mn))
      (is (= 63 mx)))))

(test image-statistics-returns-stats-array
  (with-image (img (make-gray-gradient 64 4))
    (let* ((stats (im-calc:image-statistics img))
           (s (aref stats 0)))
      (is (= 1 (length stats)))           ; one plane
      (is (= 0.0d0 (im-calc:stats-min s)))
      (is (= 255.0d0 (im-calc:stats-max s)))
      (is (< 100.0d0 (im-calc:stats-mean s) 160.0d0))
      (is (> (im-calc:stats-stddev s) 0)))))
