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
  "True if DATA contains a zero value, i.e. it looks NUL-terminated."
  (and data (position 0 data) t))

(defun attrib-data->string (data data-type)
  "Convert attribute data to a display string based on DATA-TYPE.

DATA is the sequence returned by IM-FILE:ATTRIBUTE - a vector for the
real data types, a list of complex numbers for the complex ones - so it
is indexed with ELT rather than FIRST/SECOND. DATA-TYPE is a keyword
from the IM-CFFI::DATA-TYPE enum."
  (when (and data (plusp (length data)))
    (case data-type
      (:data-type-byte (format nil "~3D" (elt data 0)))
      ((:data-type-short :data-type-ushort :data-type-int)
       (format nil "~5D" (elt data 0)))
      ((:data-type-float :data-type-double)
       (format nil "~5,2F" (elt data 0)))
      ((:data-type-cfloat :data-type-cdouble)
       (let ((value (elt data 0)))
         (format nil "~5,2F, ~5,2F" (realpart value) (imagpart value))))
      (otherwise (format nil "~A" (elt data 0))))))

(defun attrib-data->text (data)
  "Decode a NUL-terminated byte attribute into a Lisp string.

IM-FILE:ATTRIBUTE-STRING only works on an IM-IMAGE, not on the IM-FILE
handle we hold here, so string-valued file attributes are decoded from
their byte vector directly."
  (let ((end (or (position 0 data) (length data))))
    (map 'string #'code-char (subseq data 0 end))))

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
  "Print comprehensive image file information - port of PrintImageInfo
from info.lua.

Errors are deliberately not caught here: the callers are tests, and
swallowing a failure into a printed message would let a broken binding
pass unnoticed."
  (format t "IM Info~%")
  (format t "  File Name:~%    ~A~%" filename)

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
          ;; COLOR-MODE-CONFIG is the bitfield decoded by
          ;; IM:COLOR-MODE-CONFIG, i.e. a list of :COLOR-MODE-CONFIG-*
          ;; symbols - not a plist.
          (flet ((config-p (flag)
                   (if (member flag color-mode-config) "Yes" "No")))
            (format t "  Image #~A~%" (1+ i))
            (format t "    Width: ~A~%" width)
            (format t "    Height: ~A~%" height)
            (format t "    Color Space: ~A~%" color-space)
            (format t "      Has Alpha: ~A~%" (config-p :color-mode-config-alpha))
            (format t "      Is Packed: ~A~%" (config-p :color-mode-config-packed))
            (format t "      Is Top Down: ~A~%" (config-p :color-mode-config-topdown))
            (format t "    Data Type: ~A~%" data-type))

          (let ((image-size (im:image-data-size width height color-mode-config color-space data-type)))
            (multiple-value-bind (size unit) (get-size-desc image-size)
              (format t "    Data Size: ~,2F ~A~%" size unit)))

          (let ((attrib-list (im-file:attributes file)))
            (when attrib-list
              (format t "    Attributes:~%")
              (dolist (attrib-name attrib-list)
                (multiple-value-bind (attrib-data attrib-data-type)
                    (im-file:attribute file attrib-name)
                  (cond ((or (null attrib-data) (zerop (length attrib-data)))
                         (format t "      ~A: <empty>~%" attrib-name))
                        ((= (length attrib-data) 1)
                         (format t "      ~A: ~A~%"
                                 attrib-name
                                 (attrib-data->string attrib-data attrib-data-type)))
                        ((and (eq attrib-data-type :data-type-byte)
                              (find-zero attrib-data))
                         ;; String attribute
                         (format t "      ~A: ~A~%"
                                 attrib-name
                                 (attrib-data->text attrib-data)))
                        (t
                         (format t "      ~A: ~A ...~%"
                                 attrib-name
                                 (attrib-data->string attrib-data attrib-data-type)))))))))))))

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

(defun image-info-string (filename)
  "Capture the output of PRINT-IMAGE-INFO for FILENAME as a string."
  (with-output-to-string (out)
    (let ((*standard-output* out))
      (print-image-info (namestring (examples-image-path filename))))))

(test info-lena-jpg
  "Test image info extraction on lena.jpg"
  (is (examples-image-exists-p "lena.jpg") "lena.jpg should exist in test images")
  (let ((out (image-info-string "lena.jpg")))
    (is (search "Format: JPEG" out))
    (is (search "Compression: JPEG" out))
    (is (search "Image Count: 1" out))
    (is (search "Width: 208" out))
    (is (search "Height: 222" out))
    (is (search "Color Space: COLOR-SPACE-RGB" out))
    (is (search "Data Type: DATA-TYPE-BYTE" out))
    (is (search "Has Alpha: No" out))
    (is (search "Is Packed: Yes" out))
    (is (search "Is Top Down: Yes" out))
    ;; Attributes must actually be rendered, not aborted on.
    (is (search "Attributes:" out))
    ;; A string-valued (NUL-terminated byte) attribute decodes to text.
    (is (search "FileFormat: JPEG" out))
    ;; A float attribute formats through the numeric branch.
    (is (search "XResolution: 72.00" out))
    ;; An int attribute formats through the integer branch.
    (is (search "FileImageCount:     1" out))))

(test info-flower-gif
  "Test image info extraction on flower.gif"
  (is (examples-image-exists-p "flower.gif") "flower.gif should exist in test images")
  (let ((out (image-info-string "flower.gif")))
    (is (search "Format: GIF" out))
    (is (search "Compression: LZW" out))
    (is (search "Width: 184" out))
    (is (search "Height: 148" out))
    (is (search "Color Space: COLOR-SPACE-MAP" out))
    (is (search "Is Packed: No" out))
    (is (search "Attributes:" out))
    (is (search "FileFormat: GIF" out))))

(test info-rice-png
  "Test image info extraction on rice.png"
  (is (examples-image-exists-p "rice.png") "rice.png should exist in test images")
  (let ((out (image-info-string "rice.png")))
    (is (search "Format: PNG" out))
    (is (search "Compression: DEFLATE" out))
    (is (search "Width: 256" out))
    (is (search "Height: 256" out))
    (is (search "Color Space: COLOR-SPACE-GRAY" out))
    (is (search "Attributes:" out))
    ;; rice.png carries a Photoshop resource block, so its FileFormat
    ;; attribute reads TIFF even though the container is PNG. Both the
    ;; string and float branches are exercised here.
    (is (search "FileFormat: TIFF" out))
    (is (search "ResolutionUnit: DPC" out))
    (is (search "XResolution: 28.34" out))))

(test info-missing-file-signals
  "A missing file must signal, not print a swallowed error message."
  (signals error
    (image-info-string "no-such-image.png")))

(test stats-lena-jpg
  "Test image statistics calculation on lena.jpg"
  (is (examples-image-exists-p "lena.jpg") "lena.jpg should exist in test images")
  (with-image (img (im-file:image-load (namestring (examples-image-path "lena.jpg"))))
    (let ((out (with-output-to-string (out)
                 (let ((*standard-output* out))
                   (print-image-stats img)))))
      (is (search "min:" out))
      (is (search "mean:" out))
      (is (search "max:" out))
      ;; RGB image: three values per line, so the depth-1 branch is not taken.
      (is (= 3 (im-image:depth img))))))