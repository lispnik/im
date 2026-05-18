(in-package #:im-tests)

;;; Port of info.lua - Image file information and metadata display

(def-suite* info-examples
  :description "Info/metadata examples ported from Lua"
  :in examples-suite)

(defun error-str (error-code)
  "Convert IM error code to string description."
  ;; This would need to be implemented based on IM error constants
  (format nil "Error ~A" error-code))

(defun find-zero (data)
  "Check if array contains a zero value."
  (when data
    (find 0 data)))

(defun attrib-data->string (data data-type)
  "Convert attribute data to display string based on data type."
  (when (and data (> (length data) 0))
    (case data-type
      (:byte (format nil "~3D" (first data)))
      (:ushort (format nil "~5D" (first data)))
      (:int (format nil "~5D" (first data)))
      (:float (format nil "~5,2F" (first data)))
      (:cfloat (if (> (length data) 1)
                   (format nil "~5,2F, ~5,2F" (first data) (second data))
                   (format nil "~5,2F" (first data))))
      (otherwise (format nil "~A" (first data))))))

(defun get-size-desc (size)
  "Convert byte size to human-readable format."
  (cond ((< size 1024)
         (values size "b"))
        ((< size (* 1024 1024))
         (values (/ size 1024.0) "Kb"))
        (t
         (values (/ size 1024.0 1024.0) "Mb"))))

(defun file-size (filename)
  "Get file size in bytes."
  (with-open-file (stream filename :direction :input :if-does-not-exist nil)
    (if stream
        (file-length stream)
        0)))

(defun print-image-info (filename)
  "Print comprehensive image file information - port of PrintImageInfo from info.lua"
  (format t "IM Info~%")
  (format t "  File Name:~%    ~A~%" filename)

  (handler-case
      (im-file:with-open-file (file (im-file:open filename))
        (let ((file-size (file-size filename)))
          (multiple-value-bind (size unit) (get-size-desc file-size)
            (format t "  File Size: ~,2F ~A~%" size unit)))

        (multiple-value-bind (format compression image-count)
            (im-file:info file)
          (format t "  Format: ~A~%" format)
          (format t "  Compression: ~A~%" compression)
          (format t "  Image Count: ~A~%" image-count)

          (dotimes (i image-count)
            (multiple-value-bind (width height color-mode-config color-space data-type)
                (im-file:read-image-info file i)
              (format t "  Image #~A~%" (1+ i))
              (format t "    Width: ~A~%" width)
              (format t "    Height: ~A~%" height)
              (format t "    Color Space: ~A~%" color-space)
              (format t "      Has Alpha: ~A~%"
                      (if (getf color-mode-config :alpha) "Yes" "No"))
              (format t "      Is Packed: ~A~%"
                      (if (getf color-mode-config :packed) "Yes" "No"))
              (format t "      Is Top Down: ~A~%"
                      (if (getf color-mode-config :topdown) "Yes" "No"))
              (format t "    Data Type: ~A~%" data-type)

              (let ((image-size (im:image-data-size width height color-mode-config color-space data-type)))
                (multiple-value-bind (size unit) (get-size-desc image-size)
                  (format t "    Data Size: ~,2F ~A~%" size unit)))

              (let ((attrib-list (im-file:attributes file)))
                (when attrib-list
                  (format t "    Attributes:~%")
                  (dolist (attrib-name attrib-list)
                    (multiple-value-bind (attrib-data attrib-data-type)
                        (im-file:attribute file attrib-name)
                      (cond ((= (length attrib-data) 1)
                             (format t "      ~A: ~A~%"
                                     attrib-name
                                     (attrib-data->string attrib-data attrib-data-type)))
                            ((and (eq attrib-data-type :byte) (find-zero attrib-data))
                             ;; String attribute
                             (let ((str-data (im-file:attribute-string file attrib-name)))
                               (format t "      ~A: ~A~%" attrib-name str-data)))
                            (t
                             (format t "      ~A: ~A ...~%"
                                     attrib-name
                                     (attrib-data->string attrib-data attrib-data-type))))))))))))
    (error (e)
      (format t "Error: ~A~%" e))))

;;; Port of stats.lua - Image statistics calculation

(defun print-image-stats (image)
  "Print image statistics - port of myPrintStats from stats.lua"
  (let ((stats (im-calc:image-statistics image)))
    (if (= (im-image:depth image) 1)
        ;; Grayscale image
        (let ((s (aref stats 0)))
          (format t "min: ~A~%" (im-calc:stats-min s))
          (format t "mean: ~A~%" (im-calc:stats-mean s))
          (format t "max: ~A~%" (im-calc:stats-max s)))
        ;; Multi-channel image (RGB, etc.)
        (progn
          (format t "min: ~{~A ~}~%" (map 'list (lambda (s) (im-calc:stats-min s)) stats))
          (format t "mean: ~{~A ~}~%" (map 'list (lambda (s) (im-calc:stats-mean s)) stats))
          (format t "max: ~{~A ~}~%" (map 'list (lambda (s) (im-calc:stats-max s)) stats))))))

(test info-lena-jpg
  "Test image info extraction on lena.jpg"
  (let ((image-path (examples-image-path "lena.jpg")))
    (is (examples-image-exists-p "lena.jpg") "lena.jpg should exist in test images")
    ;; Test would capture output and verify key information is present
    (with-output-to-string (*standard-output*)
      (print-image-info (namestring image-path)))))

(test info-flower-gif
  "Test image info extraction on flower.gif"
  (let ((image-path (examples-image-path "flower.gif")))
    (is (examples-image-exists-p "flower.gif") "flower.gif should exist in test images")
    (with-output-to-string (*standard-output*)
      (print-image-info (namestring image-path)))))

(test info-rice-png
  "Test image info extraction on rice.png"
  (let ((image-path (examples-image-path "rice.png")))
    (is (examples-image-exists-p "rice.png") "rice.png should exist in test images")
    (with-output-to-string (*standard-output*)
      (print-image-info (namestring image-path)))))

(test stats-lena-jpg
  "Test image statistics calculation on lena.jpg"
  (let ((image-path (examples-image-path "lena.jpg")))
    (is (examples-image-exists-p "lena.jpg") "lena.jpg should exist in test images")
    (with-image (img (im-file:image-load (namestring image-path)))
      (with-output-to-string (*standard-output*)
        (print-image-stats img)))))