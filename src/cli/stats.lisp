;;;; src/cli/stats.lisp — `im stats' and `im compare'.

(in-package #:im.cli)

(defun stats/options ()
  (list
   (clingon:make-option
    :flag :long-name "histogram" :short-name #\H :key :histogram
    :description "Include the histogram (byte and 16-bit images only)")
   (clingon:make-option
    :flag :long-name "colors" :short-name #\C :key :colors
    :description "Count the distinct colours; slow on a large image")
   (clingon:make-option
    :integer :long-name "plane" :key :plane :initial-value -1
    :description "Report one plane only; default is every plane")))

(defun plane-statistics (image plane)
  (append (list :plane plane) (im:statistics image plane)))

(defun histogram-summary (image plane)
  "The histogram as (LEVEL . COUNT) pairs, omitting empty levels.

A byte histogram is 256 entries and a 16-bit one is 65536, almost all of them
zero for a real image. Printing every level buries the answer; the non-empty
ones are the answer."
  (let ((counts (im:histogram image :plane plane)))
    (loop for level below (length counts)
          for count = (aref counts level)
          when (plusp count)
            collect (list :level level :count count))))

(defun stats/handler (command)
  (apply-global-options command)
  (let ((paths (clingon:command-arguments command))
        (plane (clingon:getopt command :plane)))
    (when (null paths)
      (usage-error "stats needs at least one file. Try `im stats --help'."))
    (let ((reports
            (mapcar
             (lambda (path)
               (verbose "~&Measuring ~A~%" path)
               (im:with-image (image (im:load (pathname path)))
                 (let* ((planes (+ (im:depth image) (if (im:has-alpha-p image) 1 0)))
                        (wanted (if (minusp plane)
                                    (loop for p below planes collect p)
                                    (progn
                                      (unless (< -1 plane planes)
                                        (usage-error "~A has ~D plane~:P, no plane ~D"
                                                     path planes plane))
                                      (list plane)))))
                   (append
                    (list :pathname (pathname path)
                          :width (im:width image)
                          :height (im:height image)
                          :color-space (im:color-space image)
                          :data-type (im:data-type image)
                          :planes planes)
                    (when (clingon:getopt command :colors)
                      (list :distinct-colors (im:count-colors image)))
                    (list :statistics (mapcar (lambda (p) (plane-statistics image p))
                                              wanted))
                    (when (clingon:getopt command :histogram)
                      (list :histogram
                            (mapcar (lambda (p)
                                      (list :plane p
                                            :levels (histogram-summary image p)))
                                    wanted)))))))
             paths)))
      (emit (if (rest reports) reports (first reports))))))

(register-subcommand
 (clingon:make-command
  :name "stats"
  :description "Report per-plane statistics, and optionally a histogram"
  :usage "[--histogram] [--colors] [--plane N] FILE..."
  :options (stats/options)
  :handler (guarded #'stats/handler)))

;;; im compare ----------------------------------------------------------------

(defun compare/handler (command)
  (apply-global-options command)
  (let ((arguments (clingon:command-arguments command)))
    (unless (= 2 (length arguments))
      (usage-error "compare needs exactly two files. Try `im compare --help'."))
    (destructuring-bind (a b) arguments
      (im:with-images ((first-image (im:load (pathname a)))
                       (second-image (im:load (pathname b))))
        (unless (and (= (im:width first-image) (im:width second-image))
                     (= (im:height first-image) (im:height second-image)))
          (usage-error "cannot compare ~Dx~D with ~Dx~D; the images must match"
                       (im:width first-image) (im:height first-image)
                       (im:width second-image) (im:height second-image)))
        (let ((rms (im:rms-error first-image second-image)))
          (emit (list :first (pathname a)
                      :second (pathname b)
                      :width (im:width first-image)
                      :height (im:height first-image)
                      :rms-error rms
                      ;; SNR treats the second image as the noise reference, so
                      ;; it is only meaningful in that direction. Identical
                      ;; images have zero difference and an infinite ratio,
                      ;; which is reported as a null rather than a division.
                      :snr (if (zerop rms)
                               nil
                               (im:signal-to-noise-ratio first-image second-image))
                      :identical (zerop rms))))))))

(register-subcommand
 (clingon:make-command
  :name "compare"
  :description "Measure the difference between two images of the same size"
  :usage "FIRST SECOND"
  :handler (guarded #'compare/handler)))
