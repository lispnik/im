(in-package #:im-tests)

(def-suite geometric-suite :in im-suite
  :description "im-geometric package: resize / rotate / mirror / crop.")

(in-suite geometric-suite)

(test mirror-twice-is-identity
  (with-images ((src (make-gray-gradient 8 6))
                (m   (im-image:create 8 6 :color-space-gray :data-type-byte))
                (mm  (im-image:create 8 6 :color-space-gray :data-type-byte)))
    (im-geometric:mirror src m)
    (im-geometric:mirror m mm)
    (is (= 0 (image-byte-max-diff src mm)))))

(test flip-twice-is-identity
  (with-images ((src (make-gray-gradient 8 6))
                (f  (im-image:create 8 6 :color-space-gray :data-type-byte))
                (ff (im-image:create 8 6 :color-space-gray :data-type-byte)))
    (im-geometric:flip src f)
    (im-geometric:flip f ff)
    (is (= 0 (image-byte-max-diff src ff)))))

(test crop-takes-subrectangle
  (with-images ((src (make-gray-gradient 16 12))
                (dst (im-image:create 4 4 :color-space-gray :data-type-byte)))
    (im-geometric:crop src dst 2 1)
    (loop for y below 4 do
      (loop for x below 4 do
        (let ((expected (pixel-byte src 0 (+ x 2) (+ y 1))))
          (is (= expected (pixel-byte dst 0 x y))))))))

(test add-margins-centres-with-zero-fill
  (with-images ((src (make-gray-gradient 4 4))
                (dst (im-image:create 8 8 :color-space-gray :data-type-byte)))
    (im-geometric:add-margins src dst 2 2)
    ;; inner 4x4 matches src
    (loop for y below 4 do
      (loop for x below 4 do
        (is (= (pixel-byte src 0 x y)
               (pixel-byte dst 0 (+ x 2) (+ y 2))))))
    ;; outside is zero
    (is (= 0 (pixel-byte dst 0 0 0)))
    (is (= 0 (pixel-byte dst 0 7 7)))))

(test resize-doubles-dimensions
  (with-images ((src (make-gray-gradient 8 6))
                (dst (im-image:create 16 12 :color-space-gray :data-type-byte)))
    (im-geometric:resize src dst 1)
    (is (= 16 (im-image:width dst)))
    (is (= 12 (im-image:height dst)))
    ;; corner (0,0) pixels round-trip approximately
    (is (<= (abs (- (pixel-byte src 0 0 0) (pixel-byte dst 0 0 0))) 2))))

(test reduce-by-4-halves-each-dimension
  (with-images ((src (make-gray-gradient 16 12))
                (dst (im-image:create 8 6 :color-space-gray :data-type-byte)))
    (im-geometric:reduce-by-4 src dst)
    (is (= 8 (im-image:width dst)))
    (is (= 6 (im-image:height dst)))))

(test rotate-90-clockwise-and-counter-clockwise-cancel
  ;; CW + CCW should give back the original (with a temp same-shape buffer)
  (with-images ((src (make-gray-gradient 16 12))
                (cw  (im-image:create 12 16 :color-space-gray :data-type-byte))
                (back (im-image:create 16 12 :color-space-gray :data-type-byte)))
    (im-geometric:rotate-90 src cw :clockwise t)
    (im-geometric:rotate-90 cw back :clockwise nil)
    (is (= 0 (image-byte-max-diff src back)))))

(test calc-rotate-size-returns-positive-bounds
  (multiple-value-bind (w h)
      (im-geometric:calc-rotate-size 16 16
                                     (cos (/ pi 4))
                                     (sin (/ pi 4)))
    (is (>= w 16))
    (is (>= h 16))))
