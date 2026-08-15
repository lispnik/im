(in-package #:im-tests)

;;; Image Processing Examples - Ported from Lua examples

(def-suite* processing-examples
  :description "Image processing examples ported from Lua"
  :in examples-suite)

;;; Port of sobel.lua - Sobel edge detection

(test sobel-edge-detection
  "Test Sobel edge detection - port of sobel.lua"
  (is (examples-image-exists-p "lena.jpg") "lena.jpg should exist in test images")
  (with-example-output (output-path "sobel_lena" "png")
    (with-image (image (im-file:image-load (namestring (examples-image-path "lena.jpg"))))
      (with-image (filter (im-image:create-based image))
        (im-convolve:sobel image filter)

        (im-file:image-save output-path "PNG" filter)
        (is (uiop:file-exists-p output-path) "Sobel output file should be created")

        ;; The filter must have actually changed the data; an all-zero
        ;; or untouched destination would still write a valid PNG.
        (is (plusp (image-byte-max-diff image filter))
            "Sobel output should differ from the source")

        ;; And it must round-trip back at the same geometry.
        (with-image (reloaded (im-file:image-load (namestring output-path)))
          (is (im-image:match-size-p filter reloaded)))))))

;;; Port of canny.lua - Canny edge detection

(test canny-edge-detection
  "Test Canny edge detection - port of canny.lua"
  (is (examples-image-exists-p "lena.jpg") "lena.jpg should exist in test images")
  (with-example-output (output-path "canny_lena" "png")
    (with-images ((image (im-file:image-load (namestring (examples-image-path "lena.jpg"))))
                  (gray (im-image:create (im-image:width image)
                                         (im-image:height image)
                                         :color-space-gray
                                         :data-type-byte)))
      ;; Convert to grayscale - the conversion lives in IM-CONVERT, not
      ;; IM-PROCESS.
      (im-convert:to-color-space image gray)

      (with-images ((edges (im-image:create-based gray))
                    (result (im-image:create (im-image:width gray)
                                             (im-image:height gray)
                                             :color-space-binary
                                             :data-type-byte)))
        ;; IM-CONVOLVE:CANNY fills a caller-supplied destination.
        (im-convolve:canny gray edges 1.4)

        ;; Estimate thresholds, then apply hysteresis.
        (multiple-value-bind (low-level high-level)
            (im-threshold:hysteresis-estimate edges)
          (is (<= 0 low-level))
          (is (<= low-level high-level))
          (im-threshold:hysteresis edges result low-level high-level))

        (im-file:image-save output-path "PNG" result)
        (is (uiop:file-exists-p output-path) "Canny output file should be created")
        (is (plusp (count-set-pixels result))
            "Canny should detect at least some edge pixels")))))

;;; Port of process.lua - Various image processing operations
;;;
;;; The histogram section of process.lua is not ported: IM-CALC has no
;;; histogram binding yet (see the TODO in process/statistics.lisp), and
;;; IM-RENDER does not export the render-op entry point the Lua original
;;; uses to draw one.

(test split-and-merge-components
  "Split RGB into planes and merge them back - port of process.lua"
  (is (examples-image-exists-p "lena.jpg") "lena.jpg should exist in test images")
  (with-image (image (im-file:image-load (namestring (examples-image-path "lena.jpg"))))
    (let ((width (im-image:width image))
          (height (im-image:height image))
          (data-type (im-image:data-type image)))
      (with-images ((r (im-image:create width height :color-space-gray data-type))
                    (g (im-image:create width height :color-space-gray data-type))
                    (b (im-image:create width height :color-space-gray data-type))
                    (rgb (im-image:clone image)))
        ;; SPLIT-COMPONENTS takes the destinations as &rest arguments.
        (im-color:split-components image r g b)

        (with-example-output (r-path "lena_r" "png")
          (im-file:image-save r-path "PNG" r)
          (is (uiop:file-exists-p r-path) "R component should be saved"))
        (with-example-output (g-path "lena_g" "png")
          (im-file:image-save g-path "PNG" g)
          (is (uiop:file-exists-p g-path) "G component should be saved"))
        (with-example-output (b-path "lena_b" "png")
          (im-file:image-save b-path "PNG" b)
          (is (uiop:file-exists-p b-path) "B component should be saved"))

        ;; MERGE-COMPONENTS takes a list of sources.
        (im-color:merge-components (list r g b) rgb)
        (is (= 0 (image-byte-max-diff image rgb))
            "Split then merge should round-trip losslessly")

        (with-example-output (rgb-path "lena_rgb" "png")
          (im-file:image-save rgb-path "PNG" rgb)
          (is (uiop:file-exists-p rgb-path) "Merged RGB should be saved"))))))

(test replace-color-operation
  "Replace one colour with another - port of process.lua"
  (is (examples-image-exists-p "lena.jpg") "lena.jpg should exist in test images")
  (with-images ((image (im-file:image-load (namestring (examples-image-path "lena.jpg"))))
                (replaced (im-image:duplicate image)))
    ;; Colours are passed as sequences of component values.
    (im-color:replace-color image replaced #(253 189 177) #(255 0 255))
    (with-example-output (replace-path "lena_replace" "png")
      (im-file:image-save replace-path "PNG" replaced)
      (is (uiop:file-exists-p replace-path) "Color replace result should be saved"))))

(test bit-mask-operation
  "Apply a bitwise mask - port of process.lua"
  (is (examples-image-exists-p "lena.jpg") "lena.jpg should exist in test images")
  (with-images ((image (im-file:image-load (namestring (examples-image-path "lena.jpg"))))
                (masked (im-image:duplicate image)))
    ;; BIT-MASK lives in IM-ARITHMETIC and takes an unsigned-char mask
    ;; plus a :BITWISE-OP-* keyword.
    (im-arithmetic:bit-mask image masked #b01111010 :bitwise-op-xor)
    (is (plusp (image-byte-max-diff image masked))
        "XOR with a non-zero mask should change the data")
    (with-example-output (bitmask-path "lena_bitmask" "png")
      (im-file:image-save bitmask-path "PNG" masked)
      (is (uiop:file-exists-p bitmask-path) "Bit mask result should be saved"))))
