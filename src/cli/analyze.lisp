;;;; src/cli/analyze.lisp — `im analyze', region labelling and measurement.

(in-package #:im.cli)

;;; What to measure -----------------------------------------------------------
;;;
;;; Area and centroid are the default because they were the whole of this
;;; command before there was anything else, and a report that changed shape
;;; when the library grew would break every script reading it. The rest are
;;; asked for by name, or all at once with --measure all.

(defparameter *measurements*
  '(("area"      . "Pixel count")
    ("centroid"  . "Centre of mass, as x and y")
    ("bbox"      . "Bounding box: xmin, xmax, ymin, ymax, inclusive")
    ("hull"      . "Convex hull area and perimeter")
    ("feret"     . "Caliper diameters: the longest and the narrowest, with angles")
    ("intensity" . "Statistics of the source image under each region"))
  "The measurement names --measure accepts, and what each reports.")

(defun parse-measurements (text)
  "The measurement names in TEXT, validated. TEXT is comma separated, or `all'.

Whitespace around a name is dropped rather than rejected: this is documented
as a comma-separated list, `area, centroid' is how anyone writes one, and the
MCP tool hands a model the same description -- so refusing the space turns the
natural spelling into an error result."
  (let* ((trimmed (string-trim '(#\Space #\Tab) text))
         (names (if (string-equal trimmed "all")
                    (mapcar #'car *measurements*)
                    (remove "" (mapcar (lambda (name)
                                         (string-trim '(#\Space #\Tab) name))
                                       (split-commas (string-downcase trimmed)))
                            :test #'string=))))
    (dolist (name names names)
      (unless (assoc name *measurements* :test #'string=)
        (usage-error "unknown measurement ~S; try ~{~A~^, ~} or all"
                     name (mapcar #'car *measurements*))))))

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
    :flag :long-name "watershed" :key :watershed
    :description "Split touching objects with a watershed instead of labelling them as one")
   (clingon:make-option
    :string :long-name "measure" :key :measure :initial-value "area,centroid"
    :description "Measurements to report, comma separated, or `all'")
   (clingon:make-option
    :flag :long-name "list-measures" :key :list-measures
    :description "List the available measurements and exit")
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

(defun measure-regions (labelled source count measurements)
  "A list of per-region plists, one per region, carrying the MEASUREMENTS asked for.

SOURCE is the image the regions were found in, and is read only by
`intensity' -- every other measurement describes the labelled shape alone."
  (flet ((wanted (name) (member name measurements :test #'string=)))
    (let ((areas (when (wanted "area") (im:region-areas labelled count)))
          (centroids (when (wanted "centroid") (im:region-centroids labelled count)))
          (boxes (when (wanted "bbox") (im:region-bounding-boxes labelled count)))
          (hulls (when (wanted "hull") (im:region-convex-hulls labelled count)))
          (ferets (when (wanted "feret") (im:region-feret-diameters labelled count)))
          (intensities (when (wanted "intensity")
                         (im:region-intensities labelled source count))))
      (loop for i below count
            collect
            (append
             (list :region i)
             (when areas (list :area (aref areas i)))
             (when centroids
               (list :x (car (aref centroids i)) :y (cdr (aref centroids i))))
             (when boxes
               (let ((box (aref boxes i)))
                 (list :xmin (getf box :xmin) :xmax (getf box :xmax)
                       :ymin (getf box :ymin) :ymax (getf box :ymax))))
             (when hulls
               ;; Area and perimeter as IM measures them, and nothing derived
               ;; from them. Solidity -- area divided by hull area -- is the
               ;; reason to ask for a hull at all, but the two are not in the
               ;; same units: the region area is a count of pixels and the
               ;; hull area is the area of a polygon through their centres,
               ;; which is smaller by roughly half the boundary. On a rice
               ;; grain of 104 pixels the hull comes to 91, and the ratio to
               ;; report as a solidity is 1.14 -- a number solidity cannot
               ;; take. Report both figures and leave the comparison to
               ;; someone who has decided what to do about that.
               (let ((hull (aref hulls i)))
                 (list :hull-area (getf hull :area)
                       :hull-perimeter (getf hull :perimeter))))
             (when ferets
               (let ((feret (aref ferets i)))
                 (list :max-feret (getf feret :max)
                       :max-feret-angle (getf feret :max-angle)
                       :min-feret (getf feret :min)
                       :min-feret-angle (getf feret :min-angle))))
             (when intensities
               (let ((intensity (aref intensities i)))
                 (list :intensity-min (getf intensity :min)
                       :intensity-max (getf intensity :max)
                       :intensity-mean (getf intensity :mean)
                       :intensity-stddev (getf intensity :stddev)
                       :intensity-sum (getf intensity :sum)))))))))

(defun analyze-one (path &key threshold connectivity keep-border watershed
                             measurements limit)
  (verbose "~&Analysing ~A~%" path)
  ;; Under --verbose, report progress the way `im process' does. Labelling a
  ;; large image and measuring its regions is the slowest thing this tool
  ;; does -- every measurement is at least one pass over the samples -- and it
  ;; was the one long operation that ran silently.
  (call-with-progress (lambda () (analyze-image path
                                                :threshold threshold
                                                :connectivity connectivity
                                                :keep-border keep-border
                                                :watershed watershed
                                                :measurements measurements
                                                :limit limit))))

(defun analyze-image (path &key threshold connectivity keep-border watershed
                                measurements limit)
  (im:with-image (source (im:load (pathname path)))
    ;; `intensity' measures the source image under each region, and IM needs
    ;; that image to be one it can index a plane of: a colour source is
    ;; measured on its gray conversion rather than refused.
    (let ((measured (if (and (member "intensity" measurements :test #'string=)
                             (not (eq :color-space-gray (im:color-space source))))
                        (let ((g (im:create-based source :color-space :color-space-gray)))
                          (im:convert-color-space source g)
                          g)
                        source))
          (binary nil))
      ;; BINARISE inside the unwind-protect, not beside MEASURED in the LET: a
      ;; bad --threshold makes it signal, and bound there it would take the
      ;; gray conversion with it before anything was arranged to free it.
      (unwind-protect
           (progn
             (setf binary (binarise source threshold))
             (im:with-image (labelled (im:make-label-image binary))
               (let ((count (nth-value
                             1 (if watershed
                                   (im:watershed-segment binary labelled
                                                         :connectivity connectivity)
                                   (im:find-regions binary labelled
                                                    :connectivity connectivity
                                                    :touch-border keep-border)))))
                 (list :pathname (pathname path)
                       :method (if watershed :watershed :connected-components)
                       :region-count count
                       :regions
                       ;; --limit truncates the report and NOT the measurement,
                       ;; deliberately, having briefly done both.
                       ;;
                       ;; Measuring only the first LIMIT regions is correct
                       ;; against tecgraf-im v2.2.1, where the measurements
                       ;; range-check the label they index by. Against v2.2.0 it
                       ;; is an out-of-bounds write, and v2.2.0 is the dangerous
                       ;; one: every symbol this binding needs resolves there, so
                       ;; nothing fails loudly, and the binding cannot tell the
                       ;; two apart because tecgraf-im does not bump
                       ;; IM_VERSION_NUMBER. That would make the DEFAULT
                       ;; invocation -- no flags, limit 20 -- corrupt the heap on
                       ;; any image with more than twenty regions.
                       ;;
                       ;; The saving was about a tenth on a 1024x1024 image of
                       ;; 740 regions, because the cost is dominated by passes
                       ;; over the samples rather than by per-region work. Not
                       ;; worth a silent memory-corruption mode on the command
                       ;; everyone runs first. README.md states the minimum; this
                       ;; does not stake the default path on it.
                       (when (plusp count)
                         (let ((regions (measure-regions labelled measured count
                                                         measurements)))
                           (if (plusp limit)
                               (subseq regions 0 (min count limit))
                               regions)))))))
        ;; BINARY is NIL when BINARISE itself signalled; MEASURED is SOURCE
        ;; when no conversion was needed, and WITH-IMAGE owns that one.
        (when binary (im:destroy binary))
        (unless (eq measured source) (im:destroy measured))))))

(defun analyze/handler (command)
  (apply-global-options command)
  (when (clingon:getopt command :list-measures)
    (emit-table (loop for (name . description) in *measurements*
                      collect (list name description))
                :headers '("MEASURE" "REPORTS"))
    (return-from analyze/handler))
  (let ((paths (clingon:command-arguments command))
        (connectivity (clingon:getopt command :connectivity))
        (watershed (clingon:getopt command :watershed))
        (keep-border (clingon:getopt command :keep-border)))
    (when (null paths)
      (usage-error "analyze needs at least one file. Try `im analyze --help'."))
    (unless (member connectivity '(4 8))
      (usage-error "connectivity must be 4 or 8, got ~D" connectivity))
    ;; imProcessWatershedSegment always includes objects touching the border
    ;; and has nowhere to say otherwise, so --keep-border is not merely
    ;; redundant with it, it is a request the segmentation cannot refuse.
    ;; Saying so beats reporting border objects under a flag that asked for
    ;; them to be dropped.
    (when (and watershed (not keep-border))
      (verbose "~&--watershed always includes regions touching the border~%"))
    (let* ((measurements (parse-measurements (clingon:getopt command :measure)))
           (reports (mapcar (lambda (path)
                              (analyze-one path
                                           :threshold (clingon:getopt command :threshold)
                                           :connectivity connectivity
                                           :keep-border keep-border
                                           :watershed watershed
                                           :measurements measurements
                                           :limit (clingon:getopt command :limit)))
                            paths)))
      (emit (if (rest reports) reports (first reports))))))

(register-subcommand
 (clingon:make-command
  :name "analyze"
  :description "Label connected regions and measure them"
  :usage "[--threshold otsu|LEVEL] [--connectivity 4|8] [--watershed] [--measure LIST] [--limit N] FILE..."
  :options (analyze/options)
  :handler (guarded #'analyze/handler)))
