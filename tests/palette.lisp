(in-package #:im-tests)

(def-suite palette-suite :in im-suite
  :description "im-palette package: named generators, find-nearest, lifecycle.")

(in-suite palette-suite)

(defun decode-rgb (color)
  "Split a packed 24-bit IM color into (R G B) bytes."
  (values (ldb (byte 8 16) color)
          (ldb (byte 8 8) color)
          (ldb (byte 8 0) color)))

(test gray-palette-is-monochrome-ramp
  (let ((p (im-palette:gray)))
    (let ((entries (im-palette:sequence p)))
      (is (= 256 (length entries)))
      (loop for i below 256 do
        (multiple-value-bind (r g b) (decode-rgb (aref entries i))
          (is (= i r))
          (is (= i g))
          (is (= i b)))))))

(test red-palette-isolates-red-channel
  (let ((p (im-palette:red)))
    (let ((entries (im-palette:sequence p)))
      (loop for i below 256 do
        (multiple-value-bind (r g b) (decode-rgb (aref entries i))
          (is (= i r))
          (is (zerop g))
          (is (zerop b)))))))

(test named-palettes-instantiate
  (dolist (factory (list #'im-palette:rainbow
                         #'im-palette:hues
                         #'im-palette:blue-ice
                         #'im-palette:hot-iron
                         #'im-palette:black-body
                         #'im-palette:high-contrast
                         #'im-palette:linear
                         #'im-palette:uniform))
    (let ((p (funcall factory)))
      (is (= 256 (im-palette:count p))))))

(test find-nearest-on-gray-ramp-returns-index
  (let ((p (im-palette:gray)))
    ;; ColorEncode in the C API is (R<<16) | (G<<8) | B for byte values
    (flet ((rgb (r g b) (logior (ash r 16) (ash g 8) b)))
      (is (= 0   (im-palette:find-nearest p (rgb 0 0 0))))
      (is (= 100 (im-palette:find-nearest p (rgb 100 100 100))))
      (is (= 255 (im-palette:find-nearest p (rgb 255 255 255)))))))

(test new-from-sequence-roundtrip
  (let* ((entries (vector
                   (logior (ash 255 16))      ; pure red
                   (ash 255 8)                ; pure green
                   255))                      ; pure blue
         ;; The function is defined but not currently exported as
         ;; NEW-FROM-SEQUENCE; fall through to the internal symbol.
         (p (im-palette::new-from-sequence entries)))
    (is (= 3 (im-palette:count p)))
    (let ((reread (im-palette:sequence p)))
      (loop for i below 3 do
        (is (= (aref entries i) (aref reread i)))))))
