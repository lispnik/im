(defpackage #:im-process
  (:use #:common-lisp
	#:cffi
	#:serapeum)
  (:import-from #:im-process-cffi #:counter-aborted)
  (:export #:counter-aborted))

(in-package #:im-process)


(defun render-add-speckle-noise (src-im-image percent &optional dst-im-image)
  (if dst-im-image
      (im-process-cffi::%im-process-render-add-speckle-noise src-im-image dst-im-image percent)
      (let ((dst-image (im-image:create-based ))))))
