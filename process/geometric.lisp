(in-package #:im-process)

(defun mirror (src-im-image &optional (dst-im-image src-im-image))
  (im-process-cffi::%im-process-mirror src-im-image dst-im-image))
