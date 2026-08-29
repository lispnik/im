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
