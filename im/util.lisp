(in-package #:im)

(export '(color-mode-space-name
          color-mode-to-bitmap
          color-mode-bitmap-p
          data-type-size
          data-type-name
          data-type-int-max
          data-type-int-min
          color-space=
          color-mode-space
          color-mode=
          color-mode-alpha-p
          color-mode-packed-p
          color-mode-top-down-p
          +maximum-depth+))

(defalias color-mode-space-name #'im-cffi::%im-color-mode-space-name)
(defalias color-mode-to-bitmap #'im-cffi::%im-color-mode-space-name)
(defalias color-mode-bitmap-p #'im-cffi::%im-color-mode-is-bitmap)

(defalias data-type-size #'im-cffi::%im-data-type-size)
(defalias data-type-name #'im-cffi::%im-data-type-name)
(defalias data-type-int-max #'im-cffi::%im-data-type-int-max)
(defalias data-type-int-min #'im-cffi::%im-data-type-int-min)

(defun color-mode-space (color-mode)
  (logand color-mode #xff))

(defun color-space= (color-space1 color-space2)
  (= color-space1 color-space2))

(defun color-mode= (color-mode1 color-mode2)
  (color-space= (color-mode-space color-mode1)
                (color-mode-space color-mode2)))

(defun color-mode-alpha-p (color-mode)
  (zerop (lognand color-mode im-cffi::color-mode-config-alpha)))

(defun color-mode-packed-p (color-mode)
  (zerop (lognand color-mode im-cffi::color-mode-config-packed)))

(defun color-mode-top-down-p (color-mode)
  (zerop (lognand color-mode im-cffi::color-mode-config-topdown)))

(defconstant +maximum-depth+ 5)


