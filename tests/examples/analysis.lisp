(in-package #:im-tests)

;;; Image Analysis Examples - Ported from Lua examples

(def-suite* analysis-examples
  :description "Image analysis examples ported from Lua"
  :in examples-suite)

;;; Port of analyze.lua - Region analysis and measurements

(test region-analysis
  "Test region analysis and measurements - port of analyze.lua"
  (is (examples-image-exists-p "rice.png") "rice.png should exist in test images")

  (with-image (image (im-file:image-load (namestring (examples-image-path "rice.png"))))
    ;; The original script requires a grayscale byte image.
    (is (eq (im-image:color-space image) :color-space-gray)
        "rice.png should be grayscale for this analysis")
    (is (eq (im-image:data-type image) :data-type-byte)
        "rice.png should be byte type for this analysis")

    (with-images ((binary (im-image:create-based image nil nil :color-space-binary nil))
                  (region (im-image:create-based image nil nil :color-space-gray :data-type-ushort)))

      ;; Make it binary using percent threshold (lots of background).
      (im-threshold:percent image binary 70)

      ;; Search for closed regions; regions touching the border are
      ;; dropped, which is the TOUCH-BORDER NIL default.
      (let ((count (im-analyze:find-regions binary region :connect 4)))
        (is (plusp count) "Should find at least some regions")

        ;; MEASURE-PRINCIPAL-AXIS needs the centroids as well as the
        ;; areas, and every measurement comes back as a vector.
        (let ((areas (im-analyze:measure-area region count)))
          (multiple-value-bind (centroid-x centroid-y)
              (im-analyze:measure-centroid region areas count)
            (multiple-value-bind (major-slopes major-lengths minor-slopes minor-lengths)
                (im-analyze:measure-principal-axis region areas centroid-x centroid-y count)
              (declare (ignore major-slopes minor-slopes))

              ;; Report in the same shape as the Lua version. Captured
              ;; rather than printed so the suite output stays readable.
              (let ((report
                      (with-output-to-string (out)
                        (format out "~10A~15A~15A~15A~%"
                                "object" "area" "major length" "minor length")
                        (dotimes (r count)
                          (format out "~10A~15A~15,5G~15,5G~%"
                                  (1+ r)
                                  (aref areas r)
                                  (aref major-lengths r)
                                  (aref minor-lengths r))))))
                (is (search "major length" report))
                (is (= (1+ count) (count #\Newline report))
                    "Report should have a header plus one line per region"))

              (is (= (length areas) count) "Should have area for each region")
              (is (= (length major-lengths) count) "Should have major length for each region")
              (is (= (length minor-lengths) count) "Should have minor length for each region")

              (is (every #'plusp areas) "All areas should be positive")
              (is (every (lambda (x) (>= x 0d0)) major-lengths)
                  "All major lengths should be non-negative")
              (is (every (lambda (x) (>= x 0d0)) minor-lengths)
                  "All minor lengths should be non-negative")
              ;; The major axis is by definition at least as long as the minor.
              (is (every #'>= major-lengths minor-lengths)
                  "Major length should be >= minor length for every region")

              ;; Centroids must fall inside the image.
              (dotimes (r count)
                (is (<= 0 (aref centroid-x r) (im-image:width image)))
                (is (<= 0 (aref centroid-y r) (im-image:height image)))))))))))

(test region-holes-and-perimeter
  "Additional region measurements over the same labelled image."
  (is (examples-image-exists-p "rice.png") "rice.png should exist in test images")

  (with-image (image (im-file:image-load (namestring (examples-image-path "rice.png"))))
    (with-images ((binary (im-image:create-based image nil nil :color-space-binary nil))
                  (region (im-image:create-based image nil nil :color-space-gray :data-type-ushort)))
      (im-threshold:percent image binary 70)
      (let ((count (im-analyze:find-regions binary region :connect 4)))
        (is (plusp count))
        (let ((perimeters (im-analyze:measure-perimeter region count))
              (areas (im-analyze:measure-area region count)))
          (is (= count (length perimeters)))
          (is (every (lambda (x) (>= x 0d0)) perimeters))
          ;; IM reports a perimeter of 0 for single-pixel regions, so
          ;; only regions larger than that are required to be positive.
          (dotimes (r count)
            (when (> (aref areas r) 1)
              (is (plusp (aref perimeters r))
                  "Multi-pixel region ~A should have a positive perimeter" r)))
          (is (some #'plusp perimeters) "At least one region should have a perimeter"))
        (multiple-value-bind (hole-counts hole-areas hole-perims)
            (im-analyze:measure-holes region count)
          (is (= count (length hole-counts)))
          (is (= count (length hole-areas)))
          (is (= count (length hole-perims)))
          (is (every (lambda (x) (>= x 0)) hole-counts)))))))

;;; Additional analysis tests for other common operations.
;;;
;;; The histogram tests that used to live here were removed: IM-CALC
;;; exposes no HISTOGRAM or GRAY-HISTOGRAM binding (see the TODO in
;;; process/statistics.lisp). COUNT-COLORS covers the nearest available
;;; ground.

(test color-count-analysis
  "Test distinct-colour counting"
  (is (examples-image-exists-p "lena.jpg") "lena.jpg should exist in test images")

  (with-image (image (im-file:image-load (namestring (examples-image-path "lena.jpg"))))
    (let ((colors (im-calc:count-colors image))
          (pixels (* (im-image:width image) (im-image:height image))))
      (is (plusp colors) "Should find at least one colour")
      (is (<= colors pixels) "Cannot have more colours than pixels"))))

(test statistics-analysis
  "Test comprehensive image statistics"
  (is (examples-image-exists-p "lena.jpg") "lena.jpg should exist in test images")

  (with-image (image (im-file:image-load (namestring (examples-image-path "lena.jpg"))))
    ;; IMAGE-STATISTICS returns a vector of STATS instances, one per
    ;; plane, read through the STATS-* accessors.
    (let ((stats (im-calc:image-statistics image)))
      (is (= (length stats) 3) "Should get statistics for 3 RGB channels")

      (loop for channel-stats across stats do
        (let ((minimum (im-calc:stats-min channel-stats))
              (maximum (im-calc:stats-max channel-stats))
              (mean (im-calc:stats-mean channel-stats)))
          (is (numberp minimum) "Min should be a number")
          (is (numberp maximum) "Max should be a number")
          (is (numberp mean) "Mean should be a number")

          ;; Reasonable ranges for an 8-bit image.
          (is (>= minimum 0) "Min should be >= 0")
          (is (<= maximum 255) "Max should be <= 255")
          (is (<= minimum mean maximum) "Mean should lie between min and max"))))))
