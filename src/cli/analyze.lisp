;;;; src/cli/analyze.lisp — `im analyze', region labelling and measurement.

(in-package #:im.cli)

(defun analyze/options ()
  (list
   (clingon:make-option
    :string :long-name "threshold" :key :threshold :initial-value "otsu"
    :description "How to binarise first: a level, or `otsu'")
   (clingon:make-option
    :integer :long-name "connectivity" :key :connectivity :initial-value 8
    :description "Region connectivity, 4 or 8")
   (clingon:make-option
    :flag :long-name "keep-border" :key :keep-border
    :description "Include regions touching the image border")
   (clingon:make-option
    :integer :long-name "limit" :key :limit :initial-value 20
    :description "Report at most this many regions; 0 for all")))

(defun binarise (image threshold)
  "A binary image derived from IMAGE. The caller owns the result."
  (let ((gray (if (eq :color-space-gray (im:color-space image))
                  image
                  (let ((g (im:create-based image :color-space :color-space-gray)))
                    (im:convert-color-space image g)
                    g))))
    (unwind-protect
         (let ((binary (im:create-based gray :color-space :color-space-binary)))
           (if (string-equal threshold "otsu")
               (im:threshold-otsu gray binary)
               (im:threshold gray binary (parse-number threshold "threshold level")))
           binary)
      (unless (eq gray image) (im:destroy gray)))))

(defun analyze/handler (command)
  (apply-global-options command)
  (let ((paths (clingon:command-arguments command))
        (connectivity (clingon:getopt command :connectivity))
        (limit (clingon:getopt command :limit)))
    (when (null paths)
      (usage-error "analyze needs at least one file. Try `im analyze --help'."))
    (unless (member connectivity '(4 8))
      (usage-error "connectivity must be 4 or 8, got ~D" connectivity))
    (let ((reports
            (mapcar
             (lambda (path)
               (verbose "~&Analysing ~A~%" path)
               (im:with-image (source (im:load (pathname path)))
                 (let ((binary (binarise source (clingon:getopt command :threshold))))
                   (unwind-protect
                        (im:with-image (labelled (im:make-label-image binary))
                          (let ((count (nth-value
                                        1 (im:find-regions
                                           binary labelled
                                           :connectivity connectivity
                                           :touch-border (clingon:getopt command :keep-border)))))
                            (list :pathname (pathname path)
                                  :region-count count
                                  :regions
                                  (when (plusp count)
                                    (let ((areas (im:region-areas labelled count))
                                          (centroids (im:region-centroids labelled count)))
                                      (loop for i below (if (plusp limit)
                                                            (min count limit)
                                                            count)
                                            collect (list :region i
                                                          :area (aref areas i)
                                                          :x (car (aref centroids i))
                                                          :y (cdr (aref centroids i)))))))))
                     (im:destroy binary)))))
             paths)))
      (emit (if (rest reports) reports (first reports))))))

(register-subcommand
 (clingon:make-command
  :name "analyze"
  :description "Label connected regions and measure them"
  :usage "[--threshold otsu|LEVEL] [--connectivity 4|8] [--limit N] FILE..."
  :options (analyze/options)
  :handler (guarded #'analyze/handler)))
