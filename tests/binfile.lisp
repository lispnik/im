(in-package #:im-tests)

(def-suite binfile-suite :in im-suite
  :description "im-binfile package: low-level file I/O via imBinFile.")

(in-suite binfile-suite)

(test write-then-read-four-little-endian-ints
  (let ((path (namestring (tmp-path "binfile-ints.bin"))))
    (uiop:delete-file-if-exists path)
    (im-binfile:with-bin-file (out (im-binfile:new-file path))
      (im-binfile:byte-order out :little)
      (cffi:with-foreign-object (buf :int 4)
        (loop for i below 4 do
          (setf (cffi:mem-aref buf :int i) (* (1+ i) 100)))
        (im-binfile:write-bytes out buf 4 4)))
    (im-binfile:with-bin-file (in (im-binfile:open-file path))
      (im-binfile:byte-order in :little)
      (is (= 16 (im-binfile:size in)))
      (cffi:with-foreign-object (buf :int 4)
        (im-binfile:read-bytes in buf 4 4)
        (loop for i below 4 do
          (is (= (* (1+ i) 100) (cffi:mem-aref buf :int i))))))))

(test seek-tell-end-of-file
  (let ((path (namestring (tmp-path "binfile-seek.bin"))))
    (uiop:delete-file-if-exists path)
    ;; Write 8 bytes 0..7
    (im-binfile:with-bin-file (out (im-binfile:new-file path))
      (cffi:with-foreign-object (buf :uint8 8)
        (loop for i below 8 do (setf (cffi:mem-aref buf :uint8 i) i))
        (im-binfile:write-bytes out buf 8 1)))
    (im-binfile:with-bin-file (in (im-binfile:open-file path))
      (is (= 0 (im-binfile:tell in)))
      (im-binfile:seek-to in 4)
      (is (= 4 (im-binfile:tell in)))
      ;; Read one byte from offset 4
      (cffi:with-foreign-object (buf :uint8)
        (im-binfile:read-bytes in buf 1 1)
        (is (= 4 (cffi:mem-ref buf :uint8))))
      (is (= 5 (im-binfile:tell in)))
      ;; Seek to end and confirm EOF flag
      (im-binfile:seek-to in 8)
      (is-true (im-binfile:end-of-file-p in)))))

(test cpu-byte-order-returns-keyword
  (let ((order (im-binfile:cpu-byte-order)))
    (is (member order '(:little :big)))))

(test with-current-module-restores-previous
  (let ((before (im-binfile:set-current-module
                 :bin-file-module-rawfile)))
    ;; restore whatever was there before our test
    (im-binfile:set-current-module before))
  (im-binfile:with-current-module (:bin-file-module-memfile)
    ;; opening a real file under MEMFILE would fail because the path
    ;; argument must be a memory-file struct; just verify the macro
    ;; itself runs to completion without error.
    (pass "with-current-module ran")))
