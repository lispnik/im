(in-package #:im-tests)

(def-suite arithmetic-suite :in im-suite
  :description "im-arithmetic package: per-pixel ops, blends, bitwise, gamut.")

(in-suite arithmetic-suite)

(defun fill-uniform (im value)
  "Fill IM (gray, byte) with a constant byte VALUE."
  (let ((p (im-image:data im 0))
        (n (* (im-image:width im) (im-image:height im))))
    (loop for i below n do
      (setf (cffi:mem-aref p :unsigned-char i) value))))

(test binary-op-add-and-subtract-known-values
  (with-images ((a (im-image:create 4 4 :color-space-gray :data-type-byte))
                (b (im-image:create 4 4 :color-space-gray :data-type-byte))
                (d (im-image:create 4 4 :color-space-gray :data-type-byte)))
    (fill-uniform a 100)
    (fill-uniform b 30)
    (im-arithmetic:binary-op a b d :binary-op-add)
    (loop for i below 16 do
      (is (= 130 (cffi:mem-aref (im-image:data d 0) :unsigned-char i))))
    (im-arithmetic:binary-op a b d :binary-op-sub)
    (loop for i below 16 do
      (is (= 70 (cffi:mem-aref (im-image:data d 0) :unsigned-char i))))))

(test const-op-adds-scalar
  (with-images ((src (im-image:create 4 4 :color-space-gray :data-type-byte))
                (dst (im-image:create 4 4 :color-space-gray :data-type-byte)))
    (fill-uniform src 100)
    (im-arithmetic:const-op src 30 dst :binary-op-add)
    (loop for i below 16 do
      (is (= 130 (cffi:mem-aref (im-image:data dst 0) :unsigned-char i))))))

(test blend-const-endpoint-values
  ;; alpha=1.0 -> image1, alpha=0.0 -> image2
  (with-images ((a (im-image:create 4 4 :color-space-gray :data-type-byte))
                (b (im-image:create 4 4 :color-space-gray :data-type-byte))
                (d (im-image:create 4 4 :color-space-gray :data-type-byte)))
    (fill-uniform a 100)
    (fill-uniform b 200)
    (im-arithmetic:blend-const a b d 1.0)
    (is (= 100 (cffi:mem-aref (im-image:data d 0) :unsigned-char 0)))
    (im-arithmetic:blend-const a b d 0.0)
    (is (= 200 (cffi:mem-aref (im-image:data d 0) :unsigned-char 0)))))

(test bitwise-and-or
  (with-images ((a (im-image:create 4 4 :color-space-gray :data-type-byte))
                (b (im-image:create 4 4 :color-space-gray :data-type-byte))
                (d (im-image:create 4 4 :color-space-gray :data-type-byte)))
    (fill-uniform a #xF0)
    (fill-uniform b #x0F)
    (im-arithmetic:bitwise-op a b d :bitwise-op-and)
    (is (= 0 (cffi:mem-aref (im-image:data d 0) :unsigned-char 0)))
    (im-arithmetic:bitwise-op a b d :bitwise-op-or)
    (is (= #xFF (cffi:mem-aref (im-image:data d 0) :unsigned-char 0)))))

(test posterize-reduces-distinct-values
  (with-images ((src (im-image:create 256 1 :color-space-gray :data-type-byte))
                (dst (im-image:create 256 1 :color-space-gray :data-type-byte)))
    (let ((p (im-image:data src 0)))
      (loop for i below 256 do (setf (cffi:mem-aref p :unsigned-char i) i)))
    (im-arithmetic:posterize src dst 4)
    (let ((p (im-image:data dst 0))
          (seen (make-array 256 :initial-element nil)))
      (loop for i below 256 do (setf (aref seen (cffi:mem-aref p :unsigned-char i)) t))
      (let ((unique (count t seen)))
        (is (= 16 unique))))))

(test tone-gamut-invert-flips-monotonically
  ;; For an increasing source (gradient), the inverted result must be
  ;; monotonically decreasing.  The exact formula in IM uses the
  ;; data type's full range (not the scanned min/max) and has a small
  ;; off-by-one we don't need to lock down here.
  (with-images ((src (make-gray-gradient 64 1))
                (dst (im-image:create 64 1 :color-space-gray :data-type-byte)))
    (im-arithmetic:tone-gamut src dst :tone-gamut-invert)
    (let ((dp (im-image:data dst 0)))
      (loop for i from 1 below 64 do
        (is (>= (cffi:mem-aref dp :unsigned-char (1- i))
                (cffi:mem-aref dp :unsigned-char i))
            "dst[~A] (~A) should be >= dst[~A] (~A) after invert"
            (1- i) (cffi:mem-aref dp :unsigned-char (1- i))
            i (cffi:mem-aref dp :unsigned-char i))))))

(test bit-plane-outputs-boolean
  (with-images ((src (im-image:create 4 4 :color-space-gray :data-type-byte))
                (dst (im-image:create 4 4 :color-space-gray :data-type-byte)))
    (fill-uniform src #xAA)              ; 1010_1010
    ;; bit 1 of 0xAA is set
    (im-arithmetic:bit-plane src dst 1)
    (is (= 1 (cffi:mem-aref (im-image:data dst 0) :unsigned-char 0)))
    ;; bit 0 is clear
    (im-arithmetic:bit-plane src dst 0)
    (is (= 0 (cffi:mem-aref (im-image:data dst 0) :unsigned-char 0)))
    ;; reset mode clears the chosen bit
    (im-arithmetic:bit-plane src dst 1 :reset t)
    (is (= #xA8 (cffi:mem-aref (im-image:data dst 0) :unsigned-char 0)))))
