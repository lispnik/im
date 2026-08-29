;;;; src/cli/info.lisp — `im info' and `im library'.

(in-package #:im.cli)

(defun info/options ()
  (list
   (clingon:make-option
    :flag :long-name "attributes" :short-name #\a :key :attributes
    :description "Include each frame's attributes")
   (clingon:make-option
    :integer :long-name "frame" :short-name #\f :key :frame :initial-value -1
    :description "Report only this frame (0-based); default is all")))

(defun file-report (path &key attributes frame)
  (let* ((info (im:file-info path))
         (frames (getf info :frames))
         (selected (if (minusp frame)
                       frames
                       (list (or (nth frame frames)
                                 (usage-error "~A has no frame ~D (it has ~D)"
                                              path frame (length frames)))))))
    (list :pathname (getf info :pathname)
          :format (getf info :format)
          :compression (getf info :compression)
          :frame-count (getf info :frame-count)
          :frames (mapcar (lambda (f)
                            ;; Attributes are verbose and rarely wanted, so
                            ;; they are opt-in rather than something to page
                            ;; past on every call.
                            (loop for (key value) on f by #'cddr
                                  unless (and (eq key :attributes) (not attributes))
                                    append (list key
                                                 ;; A list of flag keywords
                                                 ;; would otherwise be read as
                                                 ;; a nested plist.
                                                 (if (eq key :color-mode-config)
                                                     (keyword-names value)
                                                     value))))
                          selected))))

(defun info/handler (command)
  (apply-global-options command)
  (let ((paths (clingon:command-arguments command)))
    (when (null paths)
      (usage-error "info needs at least one file. Try `im info --help'."))
    (let ((reports (mapcar (lambda (p)
                             (verbose "~&Reading ~A~%" p)
                             (file-report (pathname p)
                                          :attributes (clingon:getopt command :attributes)
                                          :frame (clingon:getopt command :frame)))
                           paths)))
      ;; One file gives an object, several give an array. Wrapping a single
      ;; result in a list would make the common `im info x.png --json | jq .format'
      ;; case need an index.
      (emit (if (rest reports) reports (first reports))))))

(register-subcommand
 (clingon:make-command
  :name "info"
  :description "Report format, dimensions, colour mode and attributes"
  :usage "[--attributes] [--frame N] FILE..."
  :options (info/options)
  :handler (guarded #'info/handler)))

;;; im library ----------------------------------------------------------------

(defun library/handler (command)
  (apply-global-options command)
  (emit (list :im-version (im:version)
              :im-version-date (im:version-date)
              :im-version-number (im:version-number)
              :cli-version *program-version*
              :fftw3 (im:fftw3-available-p)
              :capture (im:capture-available-p)
              :libraries (mapcar (lambda (entry)
                                   (list :library (string-downcase
                                                   (symbol-name (car entry)))
                                         :path (cdr entry)))
                                 (im:loaded-libraries)))))

(register-subcommand
 (clingon:make-command
  :name "library"
  :description "Report the IM version and which shared libraries loaded"
  :usage ""
  :handler (guarded #'library/handler)))

;;; im formats ----------------------------------------------------------------

(defun formats/options ()
  (list
   (clingon:make-option
    :flag :long-name "compressions" :short-name #\c :key :compressions
    :description "List each format's compression options")))

(defun formats/handler (command)
  (apply-global-options command)
  (let ((wanted (clingon:command-arguments command))
        (compressions (clingon:getopt command :compressions)))
    (let ((rows (loop for name in (im:format-list)
                      when (or (null wanted)
                               (member name wanted :test #'string-equal))
                        collect (destructuring-bind (description extensions sequence)
                                    (im:format-info name)
                                  (append (list :format name
                                                :description description
                                                :extensions extensions
                                                :multi-frame sequence)
                                          (when compressions
                                            (list :compressions
                                                  (im:format-compressions name))))))))
      (when (and wanted (null rows))
        (usage-error "no such format: ~{~A~^, ~}. Try `im formats'." wanted))
      (if *json*
          (emit rows)
          (if compressions
              (emit rows)
              (emit-table
               (mapcar (lambda (r) (list (getf r :format)
                                         (getf r :extensions)
                                         (if (getf r :multi-frame) "yes" "no")
                                         (getf r :description)))
                       rows)
               :headers '("FORMAT" "EXTENSIONS" "FRAMES" "DESCRIPTION")))))))

(register-subcommand
 (clingon:make-command
  :name "formats"
  :description "List the registered image formats"
  :usage "[--compressions] [FORMAT...]"
  :options (formats/options)
  :handler (guarded #'formats/handler)))
