(in-package #:im-tests)

;;; Image Processing Examples - Ported from Lua examples

(def-suite* processing-examples
  :description "Image processing examples ported from Lua"
  :in examples-suite)

;;; Port of sobel.lua - Sobel edge detection

(test sobel-edge-detection
  "Test Sobel edge detection - port of sobel.lua"
  (let ((input-path (examples-image-path "lena.jpg")))
    (is (examples-image-exists-p "lena.jpg") "lena.jpg should exist in test images")

    (with-example-output (output-path "sobel_lena" "png")
      (with-image (image (im-file:image-load (namestring input-path)))
        ;; Apply Sobel edge detection
        (with-image (filter (im-image:create-based image))
          (im-convolve:sobel image filter)
          ;; Note: Alpha channel handling could be added here if needed

          ;; Save the result
          (im-file:image-save filter (namestring output-path) "PNG")

          ;; Verify the output file was created
          (is (uiop:file-exists-p output-path) "Sobel output file should be created"))))))

;;; Port of canny.lua - Canny edge detection

(test canny-edge-detection
  "Test Canny edge detection - port of canny.lua"
  (let ((input-path (examples-image-path "lena.jpg")))
    (is (examples-image-exists-p "lena.jpg") "lena.jpg should exist in test images")

    (with-example-output (output-path "canny_lena" "png")
      (with-images ((image (im-file:image-load (namestring input-path)))
                    (gray (im-image:create (im-image:width image)
                                          (im-image:height image)
                                          :color-space-gray
                                          :data-type-byte)))
        ;; Convert to grayscale
        (im-process:convert-color-space image gray)

        ;; Apply Canny edge detection
        (let ((sigma 1.4))
          (with-image (filter (im-process:canny-new gray sigma))
            ;; Estimate thresholds
            (multiple-value-bind (low-level high-level)
                (im-process:hysteresis-threshold-estimate filter)

              ;; Apply hysteresis threshold
              (with-image (result (im-process:hysteresis-threshold-new
                                  filter
                                  (or low-level 0)
                                  (or high-level 100)))
                ;; Save the result
                (im-file:image-save result (namestring output-path) "PNG")

                ;; Verify the output file was created
                (is (uiop:file-exists-p output-path) "Canny output file should be created")))))))))

;;; Port of process.lua - Various image processing operations

(test process-operations
  "Test various processing operations - port of process.lua"
  (let ((input-path (examples-image-path "lena.jpg")))
    (is (examples-image-exists-p "lena.jpg") "lena.jpg should exist in test images")

    (with-image (image (im-file:image-load (namestring input-path)))
      ;; Test histogram calculation and saving
      (with-example-output (hist-r-path "lena_histogram_R" "gif")
        (let ((hist-r (im-calc:histogram image 0)))
          (save-histogram hist-r (namestring hist-r-path) "GIF")
          (is (uiop:file-exists-p hist-r-path) "R histogram should be created")))

      (with-example-output (hist-g-path "lena_histogram_G" "gif")
        (let ((hist-g (im-calc:histogram image 1)))
          (save-histogram hist-g (namestring hist-g-path) "GIF")
          (is (uiop:file-exists-p hist-g-path) "G histogram should be created")))

      (with-example-output (hist-b-path "lena_histogram_B" "gif")
        (let ((hist-b (im-calc:histogram image 2)))
          (save-histogram hist-b (namestring hist-b-path) "GIF")
          (is (uiop:file-exists-p hist-b-path) "B histogram should be created")))

      (with-example-output (hist-gray-path "lena_histogram_gray" "gif")
        (let ((hist-gray (im-calc:gray-histogram image)))
          (save-histogram hist-gray (namestring hist-gray-path) "GIF")
          (is (uiop:file-exists-p hist-gray-path) "Gray histogram should be created")))

      ;; Test component splitting
      (with-images ((r (im-image:create (im-image:width image)
                                       (im-image:height image)
                                       :color-space-gray
                                       (im-image:data-type image)))
                    (g (im-image:create (im-image:width image)
                                       (im-image:height image)
                                       :color-space-gray
                                       (im-image:data-type image)))
                    (b (im-image:create (im-image:width image)
                                       (im-image:height image)
                                       :color-space-gray
                                       (im-image:data-type image))))
        ;; Split RGB components
        (im-process:split-components image (list r g b))

        ;; Save individual components
        (with-example-output (r-path "lena_r" "jpg")
          (im-file:image-save r (namestring r-path) "JPEG")
          (is (uiop:file-exists-p r-path) "R component should be saved"))

        (with-example-output (g-path "lena_g" "jpg")
          (im-file:image-save g (namestring g-path) "JPEG")
          (is (uiop:file-exists-p g-path) "G component should be saved"))

        (with-example-output (b-path "lena_b" "jpg")
          (im-file:image-save b (namestring b-path) "JPEG")
          (is (uiop:file-exists-p b-path) "B component should be saved"))

        ;; Test component merging
        (with-image (rgb (im-image:clone image))
          (im-process:merge-components (list r g b) rgb)
          (with-example-output (rgb-path "lena_rgb" "jpg")
            (im-file:image-save rgb (namestring rgb-path) "JPEG")
            (is (uiop:file-exists-p rgb-path) "Merged RGB should be saved"))))

      ;; Test color replacement
      (with-image (replace (im-image:duplicate image))
        (im-process:replace-color image replace '(253 189 177) '(255 0 255))
        (with-example-output (replace-path "lena_replace" "jpg")
          (im-file:image-save replace (namestring replace-path) "JPEG")
          (is (uiop:file-exists-p replace-path) "Color replace result should be saved")))

      ;; Test bit mask operation
      (with-image (bitmask (im-image:duplicate image))
        (im-process:bit-mask image bitmask "01111010" :bit-xor)
        (with-example-output (bitmask-path "lena_bitmask" "jpg")
          (im-file:image-save bitmask (namestring bitmask-path) "JPEG")
          (is (uiop:file-exists-p bitmask-path) "Bit mask result should be saved"))))))

(defun save-histogram (hist filename format)
  "Save histogram as image - port of save_histogram function from process.lua"
  (let* ((height 200)  ; image height
         (max-val (reduce #'max hist))  ; maximum histogram value
         (n (length hist))  ; histogram size
         (white 255)
         (black 0))

    (with-image (image (im-image:create n height :color-space-gray :data-type-byte))
      ;; Render histogram
      (let ((render-func
             (lambda (x y d param)
               (declare (ignore d param))
               (let* ((v (/ (nth x hist) max-val))
                      (h (* v height)))
                 (if (<= y h) black white)))))
        (im-process:render-op image render-func "histogram"))

      ;; Save the histogram image
      (im-file:image-save image filename format))))