(in-package #:im-tests)

(def-suite morph-suite :in im-suite
  :description "im-morph package: gray and binary morphology.")

(in-suite morph-suite)

(test bin-erode-then-dilate-recovers-solid-block
  ;; Open of a solid blob = the blob (idempotent on simple shapes)
  (with-images ((src (make-binary-block))
                (opened (im-image:create 16 16 :color-space-binary :data-type-byte)))
    (im-morph:bin-open src opened 3)
    (is (= 0 (image-byte-max-diff src opened)))))

(test bin-erode-shrinks-block
  (with-images ((src (make-binary-block))
                (eroded (im-image:create 16 16 :color-space-binary :data-type-byte)))
    (let ((before (count-set-pixels src)))
      (im-morph:bin-erode src eroded 3)
      (is (< (count-set-pixels eroded) before)))))

(test bin-dilate-grows-block
  (with-images ((src (make-binary-block))
                (dilated (im-image:create 16 16 :color-space-binary :data-type-byte)))
    (let ((before (count-set-pixels src)))
      (im-morph:bin-dilate src dilated 3)
      (is (> (count-set-pixels dilated) before)))))

(test bin-outline-is-subset-of-source
  (with-images ((src (make-binary-block))
                (outline (im-image:create 16 16 :color-space-binary :data-type-byte)))
    (im-morph:bin-outline src outline 3)
    ;; every set bit of the outline must also be set in src
    (loop for i below (* 16 16) do
      (when (not (zerop (cffi:mem-aref (im-image:data outline 0) :unsigned-char i)))
        (is (not (zerop (cffi:mem-aref (im-image:data src 0) :unsigned-char i))))))
    ;; perimeter of a 4x4 block under IM's outlining = 12
    (is (= 12 (count-set-pixels outline)))))

(test gray-dilate-grows-bright-region
  (with-images ((src (im-image:create 16 16 :color-space-gray :data-type-byte))
                (dst (im-image:create 16 16 :color-space-gray :data-type-byte)))
    (let ((p (im-image:data src 0)))
      (loop for i below (* 16 16) do (setf (cffi:mem-aref p :unsigned-char i) 50))
      (loop for y from 7 below 9 do
        (loop for x from 7 below 9 do
          (setf (cffi:mem-aref p :unsigned-char (+ (* y 16) x)) 200))))
    (im-morph:gray-dilate src dst 3)
    (let ((bright-count
            (loop for i below (* 16 16)
                  count (= 200 (cffi:mem-aref (im-image:data dst 0) :unsigned-char i)))))
      (is (> bright-count 4)))))

(test gray-gradient-emphasises-edge
  (with-images ((src (im-image:create 16 16 :color-space-gray :data-type-byte))
                (dst (im-image:create 16 16 :color-space-gray :data-type-byte)))
    (let ((p (im-image:data src 0)))
      (loop for y below 16 do
        (loop for x below 16 do
          (setf (cffi:mem-aref p :unsigned-char (+ (* y 16) x))
                (if (< x 8) 0 200)))))
    (im-morph:gray-gradient src dst 3)
    ;; some edge column pixels should be non-zero
    (let ((edge-nonzero
            (loop for y below 16
                  count (or (> (pixel-byte dst 0 7 y) 0)
                            (> (pixel-byte dst 0 8 y) 0)))))
      (is (> edge-nonzero 0)))))
