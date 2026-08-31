;;;; tests/file.lisp — reading, writing and inspecting files.

(in-package #:im.tests)

(def-suite file-suite :in im-suite
  :description "File I/O, format metadata and attributes.")
(in-suite file-suite)

(test load-reads-a-real-image
  (im:with-image (image (im:load (image-file "lena.jpg")))
    (is (plusp (im:width image)))
    (is (plusp (im:height image)))
    (is (eq :color-space-rgb (im:color-space image)))))

(test file-info-reports-header-fields
  (let* ((info (im:file-info (image-file "lena.jpg")))
         (frame (first (getf info :frames))))
    (is (string= "JPEG" (getf info :format)))
    (is (= 1 (getf info :frame-count)))
    (is (plusp (getf frame :width)))
    (is (eq :color-space-rgb (getf frame :color-space)))
    (is (eq :data-type-byte (getf frame :data-type)))
    ;; A JPEG is stored packed and top-down; the config bits must survive.
    (is (member :color-mode-config-packed (getf frame :color-mode-config)))))

(test attributes-decode-text-as-strings
  "Byte attributes that hold text come back as strings, not code vectors."
  (let ((attributes (im:attributes (image-file "lena.jpg"))))
    (is (plusp (length attributes)))
    (let ((format (assoc "FileFormat" attributes :test #'string=)))
      (is-true format)
      (is (stringp (first (cdr format))))
      (is (string= "JPEG" (first (cdr format)))))))

(test image-attributes-round-trip-every-type
  "Each imDataType survives being set and read back on an image."
  (im:with-image (image (im:create 4 4 :color-space-rgb :data-type-byte))
    (setf (im:image-attribute image "Author") "Ada")
    (im:set-image-attribute image "Exposure" 250)
    (im:set-image-attribute image "Gamma" 2.2d0)
    (im:set-image-attribute image "Levels" #(1 2 3))
    (im:set-image-attribute image "Small" 200 :data-type :data-type-byte)
    (im:set-image-attribute image "Wave" (vector #C(1.0 2.0))
                            :data-type :data-type-cfloat)
    (is (string= "Ada" (im:image-attribute image "Author")))
    (is (equalp #(250) (im:image-attribute image "Exposure")))
    (is (equalp #(2.2d0) (im:image-attribute image "Gamma")))
    (is (equalp #(1 2 3) (im:image-attribute image "Levels")))
    ;; A byte attribute of one non-printable value is data, not text.
    (is (equalp #(200) (im:image-attribute image "Small")))
    ;; Complex attributes are pairs of adjacent parts in C. Decoding them as
    ;; bytes -- which is what happened before -- gives plausible small
    ;; integers rather than an error.
    (is (equalp (vector #C(1.0 2.0)) (im:image-attribute image "Wave")))
    (multiple-value-bind (value type count) (im:image-attribute image "Levels")
      (declare (ignore value))
      (is (eq :data-type-int type))
      (is (= 3 count)))))

(test image-attribute-data-types-are-inferred-narrowly
  (im:with-image (image (im:create 4 4 :color-space-rgb :data-type-byte))
    (dolist (spec '(("i" 7 :data-type-int)
                    ("r" 1.5d0 :data-type-double)
                    ("c" #C(1d0 1d0) :data-type-cdouble)))
      (destructuring-bind (name value type) spec
        (im:set-image-attribute image name value)
        (is (eq type (nth-value 1 (im:image-attribute image name)))
            "~S should be stored as ~S" value type)))))

(test image-attributes-lists-what-was-set
  (im:with-image (image (im:create 4 4 :color-space-rgb :data-type-byte))
    (im:set-image-attribute image "One" 1)
    (im:set-image-attribute image "Two" 2)
    (let ((attributes (im:image-attributes image)))
      (is (= 2 (length attributes)))
      (is (equalp #(1) (first (cdr (assoc "One" attributes :test #'string=))))))))

(test setting-an-image-attribute-to-nil-removes-it
  (im:with-image (image (im:create 4 4 :color-space-rgb :data-type-byte))
    (setf (im:image-attribute image "Author") "Ada")
    (is-true (im:image-attribute image "Author"))
    (setf (im:image-attribute image "Author") nil)
    (is (null (im:image-attribute image "Author")))
    (is (null (im:image-attributes image)))))

(test image-attributes-reject-values-they-cannot-store-exactly
  "Every one of these is a silent corruption if it is allowed through."
  (im:with-image (image (im:create 4 4 :color-space-rgb :data-type-byte))
    ;; Wraps to 44 in a byte cell.
    (signals im:data-error
      (im:set-image-attribute image "x" 300 :data-type :data-type-byte))
    ;; No IM integer type is wider than 32 bits; a double would round it.
    (signals im:data-error (im:set-image-attribute image "x" (expt 2 40)))
    (signals im:data-error
      (im:set-image-attribute image "x" "text" :data-type :data-type-int))
    (signals im:data-error (im:set-image-attribute image "x" #(1 "two")))
    ;; Zero values is IM's spelling of removal, and saying so beats storing
    ;; an attribute that reads back as absent.
    (signals im:data-error (im:set-image-attribute image "x" #()))
    ;; A complex needs a complex type: half of it would be dropped silently.
    (signals im:data-error
      (im:set-image-attribute image "x" #C(1.0 2.0) :data-type :data-type-double))
    ;; Out of range for the cell asked for.
    (signals im:data-error
      (im:set-image-attribute image "x" 1d300 :data-type :data-type-float))
    ;; No IM type holds 1/3, and rounding it into a double without saying so
    ;; contradicts what the inference promises.
    (signals im:data-error (im:set-image-attribute image "x" 1/3))
    ;; Asked for explicitly, rounding is the caller's decision to make.
    (finishes (im:set-image-attribute image "x" 1/3 :data-type :data-type-double))))

(test attribute-failures-are-all-im-errors
  "The hierarchy is the promise; CL:TYPE-ERROR and FLOATING-POINT-OVERFLOW
escaping from COERCE broke it for two of these."
  (im:with-image (image (im:create 4 4 :color-space-rgb :data-type-byte))
    (dolist (bad (list (list #C(1.0 2.0) :data-type-double)
                       (list 1d300 :data-type-float)
                       (list 300 :data-type-byte)
                       (list "text" :data-type-int)))
      (destructuring-bind (value type) bad
        (handler-case (progn (im:set-image-attribute image "x" value :data-type type)
                             (fail "~S as ~S was accepted" value type))
          (im:im-error () (pass))
          (error (c) (fail "~S as ~S signalled ~A, not an IM:IM-ERROR"
                           value type (type-of c))))))))

(test image-attributes-reach-the-file
  "An attribute set on an image is written by SAVE -- if the format knows it.

PNG keeps \"Author\". TIFF has no tag for it and drops it without a word, which
is the caveat in SET-IMAGE-ATTRIBUTE's docstring and the reason to read a file
back rather than assume: the two calls here differ only in the extension."
  (im:with-image (image (im:load (image-file "lena.jpg")))
    (setf (im:image-attribute image "Author") "Ada Lovelace")
    (let ((png (tmp-file "authored.png"))
          (tiff (tmp-file "authored.tif")))
      (im:save image png)
      (im:save image tiff)
      (is (equal '("Ada Lovelace" :data-type-byte 13) (im:attribute png "Author")))
      (is (null (im:attribute tiff "Author"))))))

(test round-trip-through-several-formats
  (im:with-image (source (im:load (image-file "lena.jpg")))
    (dolist (spec '(("rt.png" . "PNG") ("rt.tif" . "TIFF") ("rt.bmp" . "BMP")))
      (let ((path (tmp-file (car spec))))
        (im:save source path)
        (is (string= (cdr spec) (getf (im:file-info path) :format))
            "saving ~A must produce a ~A" (car spec) (cdr spec))
        (im:with-image (reloaded (im:load path))
          (is (= (im:width source) (im:width reloaded)))
          (is (= (im:height source) (im:height reloaded))))))))

(test compression-is-actually-applied
  "The requested compression reaches the file.

Setting a \"Compression\" attribute on the image looks like it should work and
does not -- IM ignores it and uses the format default. That went unnoticed
because TIFF's default is LZW, so asking for LZW appeared to succeed; asking
for NONE is what exposes it."
  (im:with-image (source (im:load (image-file "lena.jpg")))
    (dolist (compression '("NONE" "LZW" "DEFLATE"))
      (let ((path (tmp-file (format nil "c-~A.tif" compression))))
        (im:save source path :compression compression)
        (is (string= compression (getf (im:file-info path) :compression)))))))

(test compression-does-not-leak-between-saves
  (im:with-image (source (im:load (image-file "lena.jpg")))
    (im:save source (tmp-file "leak-1.tif") :compression "NONE")
    (is (null (im:image-attribute-string source "Compression"))
        "SAVE must not leave a Compression attribute on the caller's image")))

(test format-list-includes-the-built-ins
  (let ((formats (im:format-list)))
    (dolist (name '("TIFF" "JPEG" "PNG" "GIF" "BMP"))
      (is (member name formats :test #'string=) "~A must be registered" name))))

(test format-compressions-is-not-narrowed-by-default
  "The default must be IM's wildcard, not colour mode 0 (which is RGB byte)."
  (let ((all (im:format-compressions "TIFF"))
        (rgb (im:format-compressions
              "TIFF"
              :color-mode (cffi:foreign-enum-value 'im.ffi::color-space :color-space-rgb)
              :data-type (cffi:foreign-enum-value 'im.ffi::data-type :data-type-byte))))
    (is (> (length all) (length rgb))
        "TIFF supports more compressions overall than for RGB byte specifically")
    (is (member "LZW" all :test #'string=))))

(test format-can-write-p-answers-the-right-way-round
  "imFormatCanWriteImage returns an error code; zero means yes."
  (let ((rgb (cffi:foreign-enum-value 'im.ffi::color-space :color-space-rgb))
        (byte (cffi:foreign-enum-value 'im.ffi::data-type :data-type-byte)))
    (is-true (im:format-can-write-p "PNG" "DEFLATE" rgb byte))
    (is-true (im:format-can-write-p "TIFF" "LZW" rgb byte))
    ;; GIF stores indexed colour only, so RGB is not writable.
    (is-false (im:format-can-write-p "GIF" "NONE" rgb byte))))

(test multi-frame-file-reports-its-frames
  (let ((info (im:file-info (image-file "flower.gif"))))
    (is (string= "GIF" (getf info :format)))
    (is (plusp (getf info :frame-count)))
    (is (= (getf info :frame-count) (length (getf info :frames))))))
