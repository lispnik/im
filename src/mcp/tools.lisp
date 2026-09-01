;;;; src/mcp/tools.lisp — the tools the server advertises, and their work.
;;;;
;;;; Each tool reuses the image algebra already written for im(1): the
;;;; perceptual hashes and SSIM live in IM.CLI (behind `im diff'), the
;;;; compositing behind `im montage'. Reaching in with double colons keeps one
;;;; implementation of each rather than a second that can drift; if that layer
;;;; ever wants a public home it is the IM library, and both callers move
;;;; together.

(in-package #:im.mcp)

;;; JSON helpers ---------------------------------------------------------------

(defun jsonify (value)
  "Turn the plists IM's inspection functions return into shasht-writable data."
  (typecase value
    (null :null)
    ((eql t) t)
    (keyword (string-downcase (symbol-name value)))
    (string value)
    (pathname (namestring value))
    (cons
     (if (and (evenp (length value)) (keywordp (first value))
              (loop for (k nil) on value by #'cddr always (keywordp k)))
         (loop with table = (make-hash-table :test #'equal)
               for (k v) on value by #'cddr
               do (setf (gethash (string-downcase (symbol-name k)) table) (jsonify v))
               finally (return table))
         (coerce (mapcar #'jsonify value) 'vector)))
    (t value)))

(defun json-string (value)
  (remove #\Newline (with-output-to-string (s) (shasht:write-json (jsonify value) s))))

;;; base64 (for inline image content) ------------------------------------------

(defparameter +b64+ "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")

(defun base64-encode (bytes)
  "Standard base64 of a byte vector, no line breaks."
  (with-output-to-string (out)
    (loop with n = (length bytes)
          for i from 0 below n by 3
          for b0 = (aref bytes i)
          for b1 = (if (< (+ i 1) n) (aref bytes (+ i 1)) 0)
          for b2 = (if (< (+ i 2) n) (aref bytes (+ i 2)) 0)
          for triple = (logior (ash b0 16) (ash b1 8) b2)
          do (write-char (char +b64+ (ldb (byte 6 18) triple)) out)
             (write-char (char +b64+ (ldb (byte 6 12) triple)) out)
             (write-char (if (< (+ i 1) n) (char +b64+ (ldb (byte 6 6) triple)) #\=) out)
             (write-char (if (< (+ i 2) n) (char +b64+ (ldb (byte 6 0) triple)) #\=) out))))

(defun image->png-base64 (image)
  "Save IMAGE to a temporary PNG and return its bytes, base64-encoded."
  (uiop:with-temporary-file (:pathname path :type "png")
    (im:save image path)
    (let ((bytes (with-open-file (in path :element-type '(unsigned-byte 8))
                   (let ((v (make-array (file-length in) :element-type '(unsigned-byte 8))))
                     (read-sequence v in) v))))
      (base64-encode bytes))))

;;; The tools ------------------------------------------------------------------
;;; (the registry, content constructors and SCHEMA/PROP shorthand are in
;;; server.lisp, which loads first.)

(define-tool "im_info"
    "Report a file's format, dimensions, colour space, data type and frames."
    (schema (list "path" (prop "string" "Path to the image file"))
            :required '("path"))
    (args)
  (list (text-content "~A" (json-string (im:file-info (pathname (required-arg args "path")))))))

(define-tool "im_stats"
    "Per-plane statistics (min, max, mean, stddev) of an image."
    (schema (list "path" (prop "string" "Path to the image file"))
            :required '("path"))
    (args)
  (im:with-image (image (im:load (pathname (required-arg args "path"))))
    (let ((planes (loop for p below (im:depth image)
                        collect (append (list :plane p) (im:statistics image p)))))
      (list (text-content "~A" (json-string
                                (list :width (im:width image) :height (im:height image)
                                      :color-space (im:color-space image)
                                      :data-type (im:data-type image)
                                      :statistics planes)))))))

(define-tool "im_formats"
    "List the image formats this build can read and write."
    (schema nil)
    (args)
  (list (text-content "~A" (json-string (im:format-list)))))

(define-tool "im_diff"
    "Compare two images structurally: RMSE, PSNR, SSIM, perceptual-hash distance
and a one-word verdict. Works even when the two are different sizes (the
perceptual hashes still apply; the pixel metrics are then omitted)."
    (schema (list "first" (prop "string" "Path to the first image")
                  "second" (prop "string" "Path to the second image"))
            :required '("first" "second"))
    (args)
  (im:with-images ((a (im:load (pathname (required-arg args "first"))))
                   (b (im:load (pathname (required-arg args "second")))))
    (let ((match (and (= (im:width a) (im:width b)) (= (im:height a) (im:height b)))))
      (im:with-images ((ga (im:grayscale a)) (gb (im:grayscale b)))
        (let* ((dha (im.cli::%difference-hash ga)) (dhb (im.cli::%difference-hash gb))
               (aha (im.cli::%average-hash ga)) (ahb (im.cli::%average-hash gb))
               (dhd (im.cli::%hamming dha dhb))
               (rms (when match (im:rms-error a b))))
          (list (text-content "~A"
                 (json-string
                  (append
                   (list :dimensions-match match)
                   (when match
                     (list :rms-error rms
                           :psnr (if (zerop rms) nil (* 20d0 (log (/ 255d0 rms) 10d0)))
                           :ssim (im.cli::%global-ssim (im.cli::%byte-plane ga 0)
                                                       (im.cli::%byte-plane gb 0))))
                   (list :ahash-distance (im.cli::%hamming aha ahb)
                         :dhash-distance dhd
                         :verdict (im.cli::%verdict match rms dhd)))))))))))

(define-tool "im_thumbnail"
    "Make a thumbnail of an image and return it inline as a PNG. `max' is the
longest-side pixel limit (default 256)."
    (schema (list "path" (prop "string" "Path to the image file")
                  "max" (prop "integer" "Longest side in pixels (default 256)"))
            :required '("path"))
    (args)
  (let ((max (or (arg args "max") 256)))
    (im:with-image (source (im:load (pathname (required-arg args "path"))))
      (im:with-image (rgb (im.cli::%to-rgb-byte source))
        (let* ((scale (min (/ max (im:width rgb)) (/ max (im:height rgb)) 1))
               (w (max 1 (round (* (im:width rgb) scale))))
               (h (max 1 (round (* (im:height rgb) scale)))))
          (im:with-image (thumb (im:resized rgb :width w :height h))
            (list (image-content (image->png-base64 thumb))
                  (text-content "thumbnail of ~A, ~Dx~D"
                                (required-arg args "path") w h))))))))

(define-tool "im_montage"
    "Compose several images into one contact-sheet grid and return it inline as
a PNG. `columns' defaults to roughly square; `tile' is WxH per cell."
    (schema (list "paths" (obj "type" "array" "items" (obj "type" "string")
                               "description" "Paths of the images to lay out")
                  "columns" (prop "integer" "Grid columns (default: about square)")
                  "tile" (prop "string" "Tile size WxH (default 160x160)")
                  "background" (prop "integer" "Background grey 0-255 (default 255)"))
            :required '("paths"))
    (args)
  (let* ((paths (coerce (required-arg args "paths") 'list))
         (background (or (arg args "background") 255))
         (tile (or (arg args "tile") "160x160"))
         (x (position #\x tile :test #'char-equal))
         (tw (and x (parse-integer tile :end x :junk-allowed t)))
         (th (and x (parse-integer tile :start (1+ x) :junk-allowed t))))
    (unless (and tw th (plusp tw) (plusp th)) (error "tile wants WxH, e.g. 160x120"))
    (unless paths (error "montage needs at least one image path"))
    (let* ((count (length paths))
           (cols (max 1 (or (arg args "columns") (ceiling (sqrt count)))))
           (rows (ceiling count cols))
           (gap 8)
           (cw (+ gap (* cols (+ tw gap))))
           (ch (+ gap (* rows (+ th gap)))))
      (im:with-image (canvas (im:create cw ch :color-space-rgb :data-type-byte))
        (im.cli::%fill-gray canvas background)
        (loop for path in paths
              for i from 0
              for col = (mod i cols) for row = (floor i cols)
              do (im:with-image (src (im:load (pathname path)))
                   (im:with-image (thumb (im.cli::%fit-thumbnail src tw th))
                     (im.cli::%paste canvas thumb
                                     (+ gap (* col (+ tw gap)) (floor (- tw (im:width thumb)) 2))
                                     (+ gap (* row (+ th gap)) (floor (- th (im:height thumb)) 2))))))
        (list (image-content (image->png-base64 canvas))
              (text-content "contact sheet of ~D image~:P, ~D column~:P" count cols))))))
