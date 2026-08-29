;;;; src/cli/convert.lisp — `im convert'.
;;;;
;;;; Deliberately separate from `im process' even though it could be expressed
;;;; as one: converting a file is the common case and should not require
;;;; knowing the pipeline syntax.

(in-package #:im.cli)

(defun convert/options ()
  (list
   (clingon:make-option
    :string :long-name "format" :short-name #\F :key :format
    :description "Output format name; default is guessed from the extension")
   (clingon:make-option
    :string :long-name "compression" :short-name #\c :key :compression
    :description "Output compression; see `im formats --compressions'")
   (clingon:make-option
    :string :long-name "color-space" :key :color-space
    :description "Convert to this colour space (rgb, gray, binary, ...)")
   (clingon:make-option
    :string :long-name "data-type" :key :data-type
    :description "Convert to this sample type (byte, ushort, float, ...)")
   (clingon:make-option
    :flag :long-name "bitmap" :key :bitmap
    :description "Convert to a displayable 8-bit image, choosing the mapping")
   (clingon:make-option
    :integer :long-name "frame" :key :frame :initial-value 0
    :description "Which frame of a multi-image input to read")))

(defun convert/handler (command)
  (apply-global-options command)
  (let ((arguments (clingon:command-arguments command)))
    (unless (= 2 (length arguments))
      (usage-error "convert needs an input and an output file. Try `im convert --help'."))
    (destructuring-bind (input output) arguments
      (im:with-image (source (im:load (pathname input)
                                      :index (clingon:getopt command :frame)))
        (let* ((space (clingon:getopt command :color-space))
               (type (clingon:getopt command :data-type))
               (bitmap (clingon:getopt command :bitmap))
               (result source)
               (owned nil))
          (unwind-protect
               (progn
                 (when bitmap
                   ;; imConvertToBitmap does colour space and data type in one
                   ;; step, and picks a sensible mapping for float or complex
                   ;; input. Doing it first means an explicit --color-space
                   ;; after it still applies.
                   (let ((destination (im:create (im:width result) (im:height result)
                                                 (if (member (im:color-space result)
                                                             '(:color-space-gray
                                                               :color-space-binary
                                                               :color-space-map))
                                                     :color-space-gray
                                                     :color-space-rgb)
                                                 :data-type-byte)))
                     (im:convert-to-bitmap result destination)
                     (when owned (im:destroy result))
                     (setf result destination owned t)))
                 (when space
                   (let ((destination (im:create-based
                                       result :color-space (keyword-for-color-space space))))
                     (im:convert-color-space result destination)
                     (when owned (im:destroy result))
                     (setf result destination owned t)))
                 (when type
                   (let ((destination (im:create-based
                                       result :data-type (keyword-for-data-type type))))
                     (im:convert-data-type result destination)
                     (when owned (im:destroy result))
                     (setf result destination owned t)))
                 (im:save result (pathname output)
                          :format (clingon:getopt command :format)
                          :compression (clingon:getopt command :compression))
                 (let ((info (im:file-info (pathname output))))
                   (emit (list :input (pathname input)
                               :output (pathname output)
                               :format (getf info :format)
                               :compression (getf info :compression)
                               :width (im:width result)
                               :height (im:height result)
                               :color-space (im:color-space result)
                               :data-type (im:data-type result)))))
            (when owned (im:destroy result))))))))

(register-subcommand
 (clingon:make-command
  :name "convert"
  :description "Convert a file's format, compression, colour space or depth"
  :usage "INPUT OUTPUT [--format F] [--compression C] [--color-space S] [--data-type T]"
  :options (convert/options)
  :handler (guarded #'convert/handler)))
