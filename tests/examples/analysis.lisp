(in-package #:im-tests)

;;; Image Analysis Examples - Ported from Lua examples

(def-suite* analysis-examples
  :description "Image analysis examples ported from Lua"
  :in examples-suite)

;;; Port of analyze.lua - Region analysis and measurements

(test region-analysis
  "Test region analysis and measurements - port of analyze.lua"
  (let ((input-path (examples-image-path "rice.png")))
    (is (examples-image-exists-p "rice.png") "rice.png should exist in test images")

    (with-images ((image (im-file:image-load (namestring input-path)))
                  (binary (im-image:create-based image nil nil :color-space-binary nil))
                  (region (im-image:create-based image nil nil nil :data-type-ushort)))

      ;; Verify image is grayscale and byte type (as required by the original script)
      (is (eq (im-image:color-space image) :color-space-gray)
          "rice.png should be grayscale for this analysis")
      (is (eq (im-image:data-type image) :data-type-byte)
          "rice.png should be byte type for this analysis")

      ;; Make it binary using percent threshold
      (im-process:percent-threshold image binary 70)  ; lots of background

      ;; Search for closed regions, don't count objects that touch image borders
      (let ((count (im-analyze:find-regions binary region 4 0)))
        (format t "regions: ~A~%" count)
        (is (> count 0) "Should find at least some regions")

        ;; Measure areas and principal axes
        (let* ((areas (im-analyze:measure-area region count))
               (principal-data (im-analyze:measure-principal-axis region areas count)))

          (multiple-value-bind (major-slopes major-lengths minor-slopes minor-lengths)
              principal-data

            ;; Print results in same format as Lua version
            (format t "~A~15A~15A~15A~%" "object" "area" "major length" "minor length")

            (dotimes (r count)
              (format t "~A~15A~15,5G~15,5G~%"
                      (1+ r)
                      (nth r areas)
                      (nth r major-lengths)
                      (nth r minor-lengths)))

            ;; Verify we got reasonable measurements
            (is (= (length areas) count) "Should have area for each region")
            (is (= (length major-lengths) count) "Should have major length for each region")
            (is (= (length minor-lengths) count) "Should have minor length for each region")

            ;; Verify measurements are positive
            (is (every #'plusp areas) "All areas should be positive")
            (is (every #'plusp major-lengths) "All major lengths should be positive")
            (is (every #'plusp minor-lengths) "All minor lengths should be positive")))))))

;; Additional analysis tests for other common operations

(test histogram-analysis
  "Test histogram calculation and analysis"
  (let ((input-path (examples-image-path "lena.jpg")))
    (is (examples-image-exists-p "lena.jpg") "lena.jpg should exist in test images")

    (with-image (image (im-file:image-load (namestring input-path)))
      ;; Test RGB histograms
      (let ((hist-r (im-calc:histogram image 0))
            (hist-g (im-calc:histogram image 1))
            (hist-b (im-calc:histogram image 2)))

        (is (= (length hist-r) 256) "R histogram should have 256 bins")
        (is (= (length hist-g) 256) "G histogram should have 256 bins")
        (is (= (length hist-b) 256) "B histogram should have 256 bins")

        ;; Test that histogram values are reasonable
        (is (every (lambda (x) (>= x 0)) hist-r) "R histogram values should be non-negative")
        (is (every (lambda (x) (>= x 0)) hist-g) "G histogram values should be non-negative")
        (is (every (lambda (x) (>= x 0)) hist-b) "B histogram values should be non-negative"))

      ;; Test grayscale histogram
      (let ((hist-gray (im-calc:gray-histogram image)))
        (is (= (length hist-gray) 256) "Gray histogram should have 256 bins")
        (is (every (lambda (x) (>= x 0)) hist-gray) "Gray histogram values should be non-negative")))))

(test statistics-analysis
  "Test comprehensive image statistics"
  (let ((input-path (examples-image-path "lena.jpg")))
    (is (examples-image-exists-p "lena.jpg") "lena.jpg should exist in test images")

    (with-image (image (im-file:image-load (namestring input-path)))
      (let ((stats (im-calc:image-statistics image)))
        ;; For RGB image, should get stats for each channel
        (is (= (length stats) 3) "Should get statistics for 3 RGB channels")

        ;; Check that each channel has min, max, mean statistics
        (dolist (channel-stats stats)
          (is (numberp (getf channel-stats :min)) "Min should be a number")
          (is (numberp (getf channel-stats :max)) "Max should be a number")
          (is (numberp (getf channel-stats :mean)) "Mean should be a number")

          ;; Verify reasonable ranges for 8-bit image
          (is (>= (getf channel-stats :min) 0) "Min should be >= 0")
          (is (<= (getf channel-stats :max) 255) "Max should be <= 255")
          (is (>= (getf channel-stats :mean) 0) "Mean should be >= 0")
          (is (<= (getf channel-stats :mean) 255) "Mean should be <= 255"))))))