(in-package #:im-tests)

(def-suite analyze-suite :in im-suite
  :description "im-analyze package: connected-components and measurements.")

(in-suite analyze-suite)

(defun two-block-binary (&key (w 32) (h 32))
  "A binary image with 4x4 blocks at (4,4) and (20,20)."
  (let* ((img (im-image:create w h :color-space-binary :data-type-byte))
         (p (im-image:data img 0)))
    (loop for i below (* w h) do (setf (cffi:mem-aref p :unsigned-char i) 0))
    (loop for y from 4 below 8 do
      (loop for x from 4 below 8 do
        (setf (cffi:mem-aref p :unsigned-char (+ (* y w) x)) 1)))
    (loop for y from 20 below 24 do
      (loop for x from 20 below 24 do
        (setf (cffi:mem-aref p :unsigned-char (+ (* y w) x)) 1)))
    img))

(test find-regions-returns-correct-count
  (with-images ((src (two-block-binary))
                (lbl (im-image:create 32 32 :color-space-gray :data-type-ushort)))
    (let ((n (im-analyze:find-regions src lbl :touch-border t)))
      (is (= 2 n)))))

(test measure-area-returns-pixel-counts
  (with-images ((src (two-block-binary))
                (lbl (im-image:create 32 32 :color-space-gray :data-type-ushort)))
    (let* ((n (im-analyze:find-regions src lbl :touch-border t))
           (areas (im-analyze:measure-area lbl n)))
      (is (= 2 (length areas)))
      (is (= 16 (aref areas 0)))
      (is (= 16 (aref areas 1))))))

(test measure-perimeter-returns-vector
  (with-images ((src (two-block-binary))
                (lbl (im-image:create 32 32 :color-space-gray :data-type-ushort)))
    (let* ((n (im-analyze:find-regions src lbl :touch-border t))
           (perims (im-analyze:measure-perimeter lbl n)))
      (is (= 2 (length perims)))
      (loop for i below 2 do
        (is (> (aref perims i) 0))))))

(test measure-centroid-locates-block-centres
  (with-images ((src (two-block-binary))
                (lbl (im-image:create 32 32 :color-space-gray :data-type-ushort)))
    (let* ((n (im-analyze:find-regions src lbl :touch-border t))
           (areas (im-analyze:measure-area lbl n)))
      (multiple-value-bind (cx cy)
          (im-analyze:measure-centroid lbl areas n)
        (is (= 2 (length cx)))
        (is (= 2 (length cy)))
        ;; Centroids should be near (5.5, 5.5) and (21.5, 21.5).
        (let ((order-ab (< (aref cx 0) (aref cx 1))))
          (let ((a-cx (if order-ab (aref cx 0) (aref cx 1)))
                (a-cy (if order-ab (aref cy 0) (aref cy 1)))
                (b-cx (if order-ab (aref cx 1) (aref cx 0)))
                (b-cy (if order-ab (aref cy 1) (aref cy 0))))
            (is (< (abs (- a-cx 5.5)) 0.01))
            (is (< (abs (- a-cy 5.5)) 0.01))
            (is (< (abs (- b-cx 21.5)) 0.01))
            (is (< (abs (- b-cy 21.5)) 0.01))))))))

(test measure-holes-counts-internal-holes
  (with-images ((lbl (im-image:create 16 16 :color-space-gray :data-type-ushort)))
    ;; Build one labelled region: a 10x10 box with a 2x2 hole inside.
    (let ((p (im-image:data lbl 0)))
      (loop for i below (* 16 16) do (setf (cffi:mem-aref p :unsigned-short i) 0))
      (loop for y from 3 below 13 do
        (loop for x from 3 below 13 do
          (setf (cffi:mem-aref p :unsigned-short (+ (* y 16) x)) 1)))
      (loop for y from 7 below 9 do
        (loop for x from 7 below 9 do
          (setf (cffi:mem-aref p :unsigned-short (+ (* y 16) x)) 0))))
    (multiple-value-bind (count area perim)
        (im-analyze:measure-holes lbl 1)
      (declare (ignore perim))
      (is (= 1 (aref count 0)))
      (is (= 4 (aref area 0))))))

(test fill-holes-fills-interior
  (with-images ((src (im-image:create 16 16 :color-space-binary :data-type-byte))
                (dst (im-image:create 16 16 :color-space-binary :data-type-byte)))
    ;; 6x6 ring outline
    (let ((p (im-image:data src 0)))
      (loop for i below (* 16 16) do (setf (cffi:mem-aref p :unsigned-char i) 0))
      (loop for y from 5 below 11 do
        (loop for x from 5 below 11 do
          (when (or (= y 5) (= y 10) (= x 5) (= x 10))
            (setf (cffi:mem-aref p :unsigned-char (+ (* y 16) x)) 1)))))
    (im-analyze:fill-holes src dst 4)
    (is (= 1 (pixel-byte dst 0 7 7)))
    (is (= 1 (pixel-byte dst 0 8 8)))))
