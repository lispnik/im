;;;; src/cli/capture.lisp — `im capture'.

(in-package #:im.cli)

(defun capture/options ()
  (list
   (clingon:make-option
    :flag :long-name "list" :short-name #\l :key :list
    :description "List the attached capture devices and exit")
   (clingon:make-option
    :integer :long-name "device" :short-name #\d :key :device :initial-value 0
    :description "Which device to grab from")
   (clingon:make-option
    :string :long-name "color-space" :key :color-space :initial-value "rgb"
    :description "Colour space to request from the device")))

(defun capture/handler (command)
  (apply-global-options command)
  (unless (im:capture-available-p)
    (cl:error 'im:capture-error
              :detail "libim_capture is not loaded. Build IM with -DIM_BUILD_CAPTURE=ON."))
  (let ((devices (im:devices)))
    (when (clingon:getopt command :list)
      (if *json*
          (emit devices)
          (if devices
              (emit-table (mapcar (lambda (d) (list (getf d :index)
                                                    (or (getf d :description) "-")
                                                    (or (getf d :path) "-")))
                                  devices)
                          :headers '("INDEX" "DESCRIPTION" "PATH"))
              ;; Not an error. Upstream builds a stub backend on Linux that
              ;; reports no devices by design, so "none" is the correct and
              ;; expected answer there rather than a failure.
              (format t "~&No capture devices.~%")))
      (return-from capture/handler))
    (let ((arguments (clingon:command-arguments command))
          (device (clingon:getopt command :device)))
      (unless (= 1 (length arguments))
        (usage-error "capture needs an output file, or --list. Try `im capture --help'."))
      (when (null devices)
        (cl:error 'im:no-device-error))
      (unless (< -1 device (length devices))
        (usage-error "no device ~D; there ~:[is~;are~] ~D. Try `im capture --list'."
                     device (/= 1 (length devices)) (length devices)))
      (let ((output (pathname (first arguments))))
        (verbose "~&Grabbing from device ~D (~A)~%" device
                 (or (getf (nth device devices) :description) "unknown"))
        (im:with-image (frame (im:capture-frame
                               device
                               :color-space (keyword-for-color-space
                                             (clingon:getopt command :color-space))))
          (im:save frame output)
          (emit (list :output output
                      :device device
                      :description (getf (nth device devices) :description)
                      :width (im:width frame)
                      :height (im:height frame)
                      :color-space (im:color-space frame))))))))

(register-subcommand
 (clingon:make-command
  :name "capture"
  :description "List capture devices, or grab a frame from one"
  :usage "--list | [--device N] OUTPUT"
  :options (capture/options)
  :handler (guarded #'capture/handler)))
