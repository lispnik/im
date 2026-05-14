(in-package #:im-tests)

(def-suite color-suite :in im-suite
  :description "im-color package: HSI / component / palette / histogram ops.")

(in-suite color-suite)

(test split-and-merge-components-roundtrip
  ;; Split RGB into 3 gray planes, merge back, expect identity.
  (with-images ((src (make-rgb-gradient 16 12))
                (p0  (im-image:create 16 12 :color-space-gray :data-type-byte))
                (p1  (im-image:create 16 12 :color-space-gray :data-type-byte))
                (p2  (im-image:create 16 12 :color-space-gray :data-type-byte))
                (rt  (im-image:create 16 12 :color-space-rgb :data-type-byte)))
    (im-color:split-components src p0 p1 p2)
    (im-color:merge-components (list p0 p1 p2) rt)
    (is (= 0 (image-byte-max-diff src rt)))))

(test split-and-merge-hsi-roundtrip-near-equal
  (with-images ((src (make-rgb-gradient 16 12))
                (h (im-image:create 16 12 :color-space-gray :data-type-float))
                (s (im-image:create 16 12 :color-space-gray :data-type-float))
                (i (im-image:create 16 12 :color-space-gray :data-type-float))
                (rt (im-image:create 16 12 :color-space-rgb :data-type-byte)))
    (im-color:split-hsi src h s i)
    (im-color:merge-hsi h s i rt)
    ;; HSI <-> RGB is non-linear; allow a few-byte tolerance.
    (is (<= (image-byte-max-diff src rt) 4))))

(test fix-bgr-swaps-r-and-b-planes
  (with-images ((src (im-image:create 4 1 :color-space-rgb :data-type-byte))
                (dst (im-image:create 4 1 :color-space-rgb :data-type-byte)))
    (loop for i below 4 do
      (setf (cffi:mem-aref (im-image:data src 0) :unsigned-char i) 255)  ; R
      (setf (cffi:mem-aref (im-image:data src 1) :unsigned-char i) 0)    ; G
      (setf (cffi:mem-aref (im-image:data src 2) :unsigned-char i) 128)) ; B
    (im-color:fix-bgr src dst)
    (is (= 128 (pixel-byte dst 0 0 0))) ; new R = old B
    (is (= 0   (pixel-byte dst 1 0 0))) ; new G = old G
    (is (= 255 (pixel-byte dst 2 0 0))))) ; new B = old R

(test replace-color-swaps-target-pixels
  (with-images ((src (im-image:create 4 1 :color-space-rgb :data-type-byte))
                (dst (im-image:create 4 1 :color-space-rgb :data-type-byte)))
    ;; All pixels red except pixel 0 which is blue
    (loop for i below 4 do
      (setf (cffi:mem-aref (im-image:data src 0) :unsigned-char i) 255)
      (setf (cffi:mem-aref (im-image:data src 1) :unsigned-char i) 0)
      (setf (cffi:mem-aref (im-image:data src 2) :unsigned-char i) 0))
    (setf (cffi:mem-aref (im-image:data src 0) :unsigned-char 0) 0)
    (setf (cffi:mem-aref (im-image:data src 2) :unsigned-char 0) 255)
    (im-color:replace-color src dst #(255 0 0) #(0 255 0))
    ;; Red pixels become green
    (is (= 0   (pixel-byte dst 0 1 0)))
    (is (= 255 (pixel-byte dst 1 1 0)))
    (is (= 0   (pixel-byte dst 2 1 0)))
    ;; Blue pixel preserved
    (is (= 0   (pixel-byte dst 0 0 0)))
    (is (= 0   (pixel-byte dst 1 0 0)))
    (is (= 255 (pixel-byte dst 2 0 0)))))

(test quantize-rgb-uniform-produces-valid-map
  (with-images ((src (make-rgb-gradient 32 24))
                (dst (im-image:create 32 24 :color-space-map :data-type-byte)))
    (im-color:quantize-rgb-uniform src dst)
    (is (eq :color-space-map (im-image:color-space dst)))))

(test quantize-gray-uniform-reduces-levels
  (with-images ((src (make-gray-gradient 64 1))
                (dst (im-image:create 64 1 :color-space-gray :data-type-byte)))
    (im-color:quantize-gray-uniform src dst 4)
    ;; Distinct output values must be <= 4
    (let ((seen (make-array 256 :initial-element nil))
          (p (im-image:data dst 0)))
      (loop for i below 64 do
        (setf (aref seen (cffi:mem-aref p :unsigned-char i)) t))
      (is (<= (count t seen) 4)))))

(test equalize-histogram-extends-range
  (with-images ((src (im-image:create 64 1 :color-space-gray :data-type-byte))
                (dst (im-image:create 64 1 :color-space-gray :data-type-byte)))
    (let ((p (im-image:data src 0)))
      (loop for i below 64 do (setf (cffi:mem-aref p :unsigned-char i) i)))
    (im-color:equalize-histogram src dst)
    ;; After equalisation max should approach 255
    (let ((mx (loop for i below 64 maximize
                    (cffi:mem-aref (im-image:data dst 0) :unsigned-char i))))
      (is (>= mx 200)))))

(test pseudo-color-produces-rgb-with-multiple-distinct-values
  (with-images ((src (im-image:create 8 1 :color-space-gray :data-type-byte))
                (dst (im-image:create 8 1 :color-space-rgb :data-type-byte)))
    (let ((p (im-image:data src 0)))
      (loop for i below 8 do (setf (cffi:mem-aref p :unsigned-char i) (* i 32))))
    (im-color:pseudo-color src dst)
    ;; PseudoColor is HSI-based; each plane should hold >= 3 distinct values.
    (loop for plane below 3 do
      (let ((seen (make-array 256 :initial-element nil))
            (pp (im-image:data dst plane)))
        (loop for i below 8 do
          (setf (aref seen (cffi:mem-aref pp :unsigned-char i)) t))
        (is (>= (count t seen) 3))))))
