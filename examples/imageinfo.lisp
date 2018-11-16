(defpackage #:im-examples.image-info
  (:use #:common-lisp)
  (:export #:image-info))

(in-package #:im-examples.image-info)

(defun image-info ()
  (let* ((pathname (asdf:system-relative-pathname :im "examples/example1.jpg"))
	 (filename (namestring pathname)))
    (im:with-open-file (file (im:file-open filename ))
)))

