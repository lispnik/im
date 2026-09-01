;;;; src/cli/montage.lisp — `im montage', a contact sheet from many images.
;;;;
;;;; Lays the inputs out in a grid, each fitted into a fixed tile on a plain
;;;; background, and writes the grid as one image. Everything is normalised to
;;;; RGB byte so a folder of mixed formats, colour spaces and depths composes
;;;; into a single sheet.

(in-package #:im.cli)

(defun parse-tile (spec)
  "Parse a WxH tile size, e.g. \"160x160\", into (VALUES WIDTH HEIGHT)."
  (let ((x (position #\x spec :test #'char-equal)))
    (unless x
      (usage-error "--tile wants WxH, e.g. 160x120, not ~S" spec))
    (let ((width (parse-integer spec :end x :junk-allowed t))
          (height (parse-integer spec :start (1+ x) :junk-allowed t)))
      (unless (and width height (plusp width) (plusp height))
        (usage-error "--tile wants two positive numbers, e.g. 160x120, not ~S" spec))
      (values width height))))

(defun %to-rgb-byte (image)
  "A fresh RGB byte copy of IMAGE, whatever it started as.

The montage's common denominator: a gray sheet and a colour photo have to end
up in the same pixel format to share a canvas. Byte first (a 16-bit or float
source is rescaled), then RGB."
  (let ((byte (if (eq :data-type-byte (im:data-type image))
                  (im:duplicate image)
                  (let ((d (im:create-based image :data-type :data-type-byte)))
                    (im:convert-data-type image d :cast-mode :min-max)
                    d))))
    (if (eq :color-space-rgb (im:color-space image))
        byte
        (let ((rgb (im:create-based byte :color-space :color-space-rgb)))
          (unwind-protect (progn (im:convert-color-space byte rgb) rgb)
            (im:destroy byte))))))

(defun %fit-thumbnail (image tile-width tile-height)
  "An RGB byte thumbnail of IMAGE no larger than TILE-WIDTH x TILE-HEIGHT,
preserving the aspect ratio."
  (im:with-image (rgb (%to-rgb-byte image))
    (let* ((scale (min (/ tile-width (im:width rgb))
                       (/ tile-height (im:height rgb))
                       1))                       ; never upscale past the tile
           (w (max 1 (round (* (im:width rgb) scale))))
           (h (max 1 (round (* (im:height rgb) scale)))))
      (im:resized rgb :width w :height h :order 1))))

(defun %fill-gray (image level)
  "Set every plane of a byte IMAGE to LEVEL -- the sheet's background."
  (dotimes (plane (im:depth image))
    (let ((pointer (im:plane-pointer image plane))
          (count (im:pixel-count image)))
      (dotimes (i count)
        (setf (cffi:mem-aref pointer :unsigned-char i) level)))))

(defun %paste (canvas thumb x0 y0)
  "Copy THUMB onto CANVAS with its top-left at (X0, Y0), all planes.

IM stores planes bottom-up, so a pixel at top-row Y sits at linear index
(height-1-Y)*width + X. Both sides are addressed that way here, which keeps the
thumbnail upright rather than mirrored."
  (let ((cw (im:width canvas)) (ch (im:height canvas))
        (tw (im:width thumb))  (th (im:height thumb)))
    (dotimes (plane (im:depth canvas))
      (let ((src (im:plane-pointer thumb plane))
            (dst (im:plane-pointer canvas plane)))
        (dotimes (y th)
          (let ((src-row (* (- th 1 y) tw))
                (dst-row (* (- ch 1 (+ y0 y)) cw)))
            (dotimes (x tw)
              (setf (cffi:mem-aref dst :unsigned-char (+ dst-row x0 x))
                    (cffi:mem-aref src :unsigned-char (+ src-row x))))))))))

(defun montage/options ()
  (list
   (clingon:make-option
    :string :long-name "output" :short-name #\o :key :output
    :description "Write the contact sheet here (required)")
   (clingon:make-option
    :integer :long-name "columns" :short-name #\c :key :columns
    :description "Columns in the grid; default is roughly square")
   (clingon:make-option
    :string :long-name "tile" :short-name #\t :key :tile :initial-value "160x160"
    :description "Tile size WxH each image is fitted into")
   (clingon:make-option
    :integer :long-name "gap" :short-name #\g :key :gap :initial-value 8
    :description "Pixels of background between and around the tiles")
   (clingon:make-option
    :integer :long-name "background" :short-name #\b :key :background :initial-value 255
    :description "Background grey level, 0 (black) to 255 (white)")))

(defun montage/handler (command)
  (apply-global-options command)
  (let ((paths (clingon:command-arguments command))
        (output (clingon:getopt command :output))
        (columns (clingon:getopt command :columns))
        (gap (clingon:getopt command :gap))
        (background (clingon:getopt command :background)))
    (unless paths
      (usage-error "montage needs at least one image. Try `im montage --help'."))
    (unless output
      (usage-error "montage needs --output FILE to write the sheet to."))
    (unless (<= 0 background 255)
      (usage-error "--background is a grey level from 0 to 255, not ~D" background))
    (multiple-value-bind (tile-width tile-height)
        (parse-tile (clingon:getopt command :tile))
      (let* ((count (length paths))
             (cols (cond (columns (max 1 columns))
                         (t (max 1 (ceiling (sqrt count))))))
             (rows (ceiling count cols))
             (canvas-width (+ gap (* cols (+ tile-width gap))))
             (canvas-height (+ gap (* rows (+ tile-height gap)))))
        (im:with-image (canvas (im:create canvas-width canvas-height
                                          :color-space-rgb :data-type-byte))
          (%fill-gray canvas background)
          (loop for path in paths
                for index from 0
                for col = (mod index cols)
                for row = (floor index cols)
                do (verbose "~&Placing ~A~%" path)
                   (im:with-image (source (im:load (pathname path)))
                     (im:with-image (thumb (%fit-thumbnail source tile-width tile-height))
                       ;; Centre the fitted thumbnail in its tile.
                       (let ((x0 (+ gap (* col (+ tile-width gap))
                                    (floor (- tile-width (im:width thumb)) 2)))
                             (y0 (+ gap (* row (+ tile-height gap))
                                    (floor (- tile-height (im:height thumb)) 2))))
                         (%paste canvas thumb x0 y0)))))
          (im:save canvas (pathname output))
          (emit (list :output (namestring (pathname output))
                      :images count
                      :columns cols
                      :rows rows
                      :tile (format nil "~Dx~D" tile-width tile-height)
                      :width canvas-width
                      :height canvas-height)))))))

(register-subcommand
 (clingon:make-command
  :name "montage"
  :description "Arrange many images into one contact-sheet grid"
  :usage "--output SHEET [--columns N] [--tile WxH] [--gap N] [--background L] FILE..."
  :options (montage/options)
  :handler (guarded #'montage/handler)))
