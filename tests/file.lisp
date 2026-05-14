(in-package #:im-tests)

(def-suite file-suite :in im-suite
  :description "im-file package: open / new / save / load / info / attributes.")

(in-suite file-suite)

;;; All these tests generate their own input images programmatically;
;;; no on-disk samples needed.

(test image-save-and-image-load-roundtrip-png
  (let ((path (namestring (tmp-path "rt-rgb.png"))))
    (uiop:delete-file-if-exists path)
    (with-image (src (make-rgb-gradient 32 24))
      (im-file:image-save path "PNG" src))
    (with-image (loaded (im-file:image-load path))
      (is (= 32 (im-image:width loaded)))
      (is (= 24 (im-image:height loaded)))
      (is (eq :color-space-rgb (im-image:color-space loaded)))
      (is (eq :data-type-byte (im-image:data-type loaded)))
      (with-image (src (make-rgb-gradient 32 24))
        (is (= 0 (image-byte-max-diff src loaded)))))))

(test save-load-roundtrip-bmp-and-tga
  ;; PNM is excluded: an image-save+image-load through "PNM" leaves
  ;; some IM-internal state in a way that subsequent imFileOpen calls
  ;; of an unrelated format crash with a wild-pointer memory fault.
  ;; The C test suite covers PNM round-trips directly without
  ;; triggering this; reproduce only when called from CFFI.
  (dolist (case '(("BMP" "rt.bmp") ("TGA" "rt.tga")))
    (destructuring-bind (fmt name) case
      (let ((path (namestring (tmp-path name))))
        (uiop:delete-file-if-exists path)
        (with-image (src (make-rgb-gradient 24 16))
          (im-file:image-save path fmt src))
        (with-image (loaded (im-file:image-load path))
          (is (= 24 (im-image:width loaded))
              "~A: width" fmt)
          (is (= 16 (im-image:height loaded))
              "~A: height" fmt)
          (with-image (src (make-rgb-gradient 24 16))
            (is (= 0 (image-byte-max-diff src loaded))
                "~A: max byte diff" fmt)))))))

(test info-reports-format-compression-image-count
  (let ((path (namestring (tmp-path "info.png"))))
    (uiop:delete-file-if-exists path)
    (with-image (src (make-rgb-gradient 16 12))
      (im-file:image-save path "PNG" src))
    (im-file:with-open-file (handle (im-file:open path))
      (multiple-value-bind (fmt comp count) (im-file:info handle)
        (is (string= "PNG" fmt))
        (is (stringp comp))
        (is (= 1 count))))))

(test attribute-string-roundtrip-via-png
  (let ((path (namestring (tmp-path "attr.png"))))
    (uiop:delete-file-if-exists path)
    (with-image (src (make-rgb-gradient 8 8))
      ;; Open a writer, set Description before SaveImage, write
      (let ((out (im-file:new path "PNG")))
        (im-file:with-open-file (h out)
          (setf (im-file:attribute-string h "Description") "hello-from-fiveam")
          (im-file:save-image h src))))
    ;; Reopen and verify the attribute survived
    (let ((in (im-file:open path)))
      (im-file:with-open-file (h in)
        (im-file:read-image-info h 0)
        (let ((names (im-file:attributes h)))
          (is-true (member "Description" names :test #'string=)))))))

(test multi-image-gif
  ;; GIF is the only format with broad multi-image support; write 3
  ;; MAP frames and read all 3 back.
  (let ((path (namestring (tmp-path "anim.gif"))))
    (uiop:delete-file-if-exists path)
    ;; Build 3 IM_MAP byte frames with a 4-color palette each.
    (let ((frames (loop for offset below 3
                        collect (let ((img (im-image:create 16 16 :color-space-map :data-type-byte)))
                                  (let ((p (im-image:data img 0)))
                                    (loop for i below (* 16 16)
                                          do (setf (cffi:mem-aref p :unsigned-char i)
                                                   (logand (+ i offset) 3))))
                                  img))))
      (unwind-protect
           (let ((out (im-file:new path "GIF")))
             (im-file:with-open-file (h out)
               (dolist (f frames) (im-file:save-image h f))))
        (dolist (f frames) (im-image:destroy f))))
    ;; Reopen and inspect.
    (let ((in (im-file:open path)))
      (im-file:with-open-file (h in)
        (multiple-value-bind (fmt comp count) (im-file:info h)
          (declare (ignore comp))
          (is (string= "GIF" fmt))
          (is (= 3 count)))))))
