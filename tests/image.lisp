;;;; tests/image.lisp — the IMAGE class and its lifetime.

(in-package #:im.tests)

(def-suite image-suite :in im-suite
  :description "Image construction, accessors and release.")
(in-suite image-suite)

(test create-and-inspect
  (im:with-image (image (im:create 32 24 :color-space-rgb :data-type-byte))
    (is (= 32 (im:width image)))
    (is (= 24 (im:height image)))
    (is (eq :color-space-rgb (im:color-space image)))
    (is (eq :data-type-byte (im:data-type image)))
    (is (= 3 (im:depth image)))
    (is (= (* 32 24) (im:pixel-count image)))
    (is (= (* 32 24 3) (im:data-size image)))
    (is-true (im:bitmap-p image))
    (is-false (im:has-alpha-p image))))

(test planes-are-contiguous
  "data[i] is data[0] offset by i*plane_size -- one allocation, not three."
  (im:with-image (image (im:create 16 16 :color-space-rgb :data-type-byte))
    (let ((base (cffi:pointer-address (im:plane-pointer image 0)))
          (size (im:plane-size image)))
      (is (= (+ base size) (cffi:pointer-address (im:plane-pointer image 1))))
      (is (= (+ base size size) (cffi:pointer-address (im:plane-pointer image 2)))))))

(test plane-index-is-bounds-checked
  (im:with-image (image (im:create 8 8 :color-space-gray :data-type-byte))
    (signals im:im-error (im:plane-pointer image 1))
    (signals im:im-error (im:plane-pointer image -1))))

(test destroy-is-idempotent
  (let ((image (im:create 8 8 :color-space-gray :data-type-byte)))
    (is-false (im:destroyed-p image))
    (im:destroy image)
    (is-true (im:destroyed-p image))
    ;; The second call must not free the pointer again.
    (finishes (im:destroy image))
    (finishes (im:destroy image))))

(test use-after-destroy-signals
  (let ((image (im:create 8 8 :color-space-gray :data-type-byte)))
    (im:destroy image)
    (signals im:invalid-image (im:width image))
    (signals im:invalid-image (im:plane-pointer image 0))))

(test with-image-releases-on-error
  (let (captured)
    (ignore-errors
     (im:with-image (image (im:create 8 8 :color-space-gray :data-type-byte))
       (setf captured image)
       (cl:error "unwind")))
    (is-true (im:destroyed-p captured))))

(test finalizer-releases-escaped-images
  "Images that never reach a WITH-IMAGE are still freed, at GC.

Allocating without the finalizer working leaks 16 MB here; allocating with a
finalizer that double-frees crashes the process. Passing means neither."
  (dotimes (i 500)
    (im:create 128 128 :color-space-rgb :data-type-byte))
  (finishes (tg:gc :full t)))

(test duplicate-copies-data-clone-does-not
  (im:with-image (source (gray-gradient 16 16))
    (im:with-images ((copy (im:duplicate source))
                     (empty (im:clone source)))
      (is (= (im:width source) (im:width copy) (im:width empty)))
      (is (= (pixel source 0 5 5) (pixel copy 0 5 5))
          "DUPLICATE must reproduce sample values")
      (is (eq (im:data-type source) (im:data-type empty))))))

(test create-based-overrides-selectively
  (im:with-image (source (im:create 40 30 :color-space-rgb :data-type-byte))
    (im:with-image (derived (im:create-based source :color-space :color-space-gray))
      (is (= 40 (im:width derived)))
      (is (= 30 (im:height derived)))
      (is (eq :color-space-gray (im:color-space derived)))
      (is (eq :data-type-byte (im:data-type derived))))))
