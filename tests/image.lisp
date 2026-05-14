(in-package #:im-tests)

(def-suite image-suite :in im-suite
  :description "im-image package: create / destroy / match / alpha / dup / clone.")

(in-suite image-suite)

(test create-and-inspect
  (with-image (img (im-image:create 32 24 :color-space-rgb :data-type-byte))
    (is (= 32 (im-image:width img)))
    (is (= 24 (im-image:height img)))
    (is (eq :color-space-rgb (im-image:color-space img)))
    (is (eq :data-type-byte (im-image:data-type img)))
    (is (= 3 (im-image:depth img)))))

(test bitmap-classification
  (with-image (rgb (im-image:create 8 8 :color-space-rgb :data-type-byte))
    (is (im-image:bitmap-p rgb)))
  (with-image (gray-float (im-image:create 8 8 :color-space-gray :data-type-float))
    (is (not (im-image:bitmap-p gray-float)))))

(test match-helpers-distinguish-shape-color-and-type
  (with-images ((a (im-image:create 16 12 :color-space-rgb :data-type-byte))
                (same (im-image:create 16 12 :color-space-rgb :data-type-byte))
                (diff-size (im-image:create 8 12 :color-space-rgb :data-type-byte))
                (diff-cs   (im-image:create 16 12 :color-space-gray :data-type-byte))
                (diff-dt   (im-image:create 16 12 :color-space-rgb :data-type-ushort)))
    (is-true (im-image:match-p a same))
    (is-true (im-image:match-size-p a same))
    (is-true (im-image:match-color-space-p a same))
    (is-true (im-image:match-data-type-p a same))
    (is-false (im-image:match-size-p a diff-size))
    (is-false (im-image:match-color-space-p a diff-cs))
    (is-false (im-image:match-data-type-p a diff-dt))
    (is-false (im-image:match-p a diff-size))))

(defun has-alpha-p (img)
  (cffi:foreign-slot-value
   img '(:struct im-cffi::im-image-struct) 'im-cffi::has-alpha-p))

(test alpha-add-set-remove-lifecycle
  (with-image (img (make-rgb-gradient 8 8))
    (is-false (has-alpha-p img))
    (im-image:add-alpha img)
    (is-true (has-alpha-p img))
    (setf (im-image:alpha img) 200)             ; just confirm setter runs
    (im-image:remove-alpha img)
    (is-false (has-alpha-p img))
    ;; The colour planes are unchanged across the lifecycle.
    (with-image (fresh (make-rgb-gradient 8 8))
      (is (= 0 (image-byte-max-diff img fresh))))))

(test duplicate-deep-copies-data
  (with-image (src (make-gray-gradient 16 16))
    (with-image (dup (im-image:duplicate src))
      (is-true (im-image:match-p src dup))
      (is (= 0 (image-byte-max-diff src dup)))
      ;; mutate dup, original is untouched
      (setf (pixel-byte dup 0 0 0) 99)
      (is (= 0 (pixel-byte src 0 0 0)))
      (is (= 99 (pixel-byte dup 0 0 0))))))

(test clone-shares-shape-only
  (with-image (src (make-gray-gradient 8 8))
    (with-image (clone (im-image:clone src))
      (is-true (im-image:match-p src clone))
      ;; clone data is fresh-zero, gradient differs
      (is (> (image-byte-max-diff src clone) 0)))))

(test copy-plane-isolated-from-others
  (with-images ((a (make-rgb-gradient 8 8))
                (b (im-image:create 8 8 :color-space-rgb :data-type-byte)))
    (im-image:copy-plane a 1 b 0)              ; G of A -> R of B
    ;; B's plane 0 must equal A's plane 1
    (loop for i below (* 8 8) do
      (is (= (cffi:mem-aref (im-image:data a 1) :unsigned-char i)
             (cffi:mem-aref (im-image:data b 0) :unsigned-char i))))
    ;; B's plane 1 still zero
    (loop for i below (* 8 8) do
      (is (zerop (cffi:mem-aref (im-image:data b 1) :unsigned-char i))))))

(test make-binary-clamps-nonzero-to-one
  (with-image (img (im-image:create 4 1 :color-space-gray :data-type-byte))
    (let ((p (im-image:data img 0)))
      (setf (cffi:mem-aref p :unsigned-char 0) 0)
      (setf (cffi:mem-aref p :unsigned-char 1) 1)
      (setf (cffi:mem-aref p :unsigned-char 2) 200)
      (setf (cffi:mem-aref p :unsigned-char 3) 255))
    (im-image:make-binary img)
    (is (= 0 (pixel-byte img 0 0 0)))
    (is (= 1 (pixel-byte img 0 1 0)))
    (is (= 1 (pixel-byte img 0 2 0)))
    (is (= 1 (pixel-byte img 0 3 0)))))
