(in-package #:im)

(export '(image-data-size
          image-line-size
          image-line-count
          image-check-format
          color-encode
          color-decode
          color-mode-space-name
          color-mode-to-bitmap
          color-mode-bitmap-p
          data-type-size
          data-type-name
          data-type-int-max
          data-type-int-min
          color-space=
          color-mode-space
          color-mode-config
          color-mode=
          color-mode-alpha-p
          color-mode-packed-p
          color-mode-top-down-p
          +maximum-depth+))

(defun %encode-color-mode (color-mode-config color-space)
  (logior
   (cffi:foreign-bitfield-value 'im-cffi::color-mode-config color-mode-config)
   (cffi:foreign-enum-value 'im-cffi::color-space color-space)))

(defun %encode-color-mode-config (color-mode-config)
  (cffi:foreign-bitfield-value 'im-cffi::color-mode-config color-mode-config))

(defun image-data-size (width height color-mode-config color-space data-type)
  "Returns the size of the data buffer."
  (let ((color-mode (%encode-color-mode color-mode-config color-space)))
    (im-cffi::%im-image-data-size
     width
     height
     color-mode
     data-type)))

(defun image-line-size (width color-mode-config color-space data-type)
  "Returns the size of one line of the data buffer. 

This depends if the components are packed. If packed includes all
components, if not includes only one."
  (let ((color-mode (%encode-color-mode color-mode-config color-space)))
    (im-cffi::%im-image-line-size
     width
     color-mode
     data-type)))

(defun image-line-count (width color-mode-config color-space)
  "Returns the number of elements of one line of the data buffer.

This depends if the components are packed. If packed includes all
components, if not includes only one."
    (im-cffi::%im-image-line-count
     width
     (%encode-color-mode color-mode-config color-space)))

(defun image-check-format (color-mode-config color-space data-type)
  "Check if the combination COLOR-MODE-CONFIG, COLOR-SPACE and
DATA-TYPE is valid."
  (im-cffi::%im-image-check-format
   (%encode-color-mode color-mode-config color-space)
   data-type))

(defun color-encode (red green blue)
  "Encode RGB components in a long for palette usage.

Compatible with CD library definition."
  (im-cffi::%im-color-encode red green blue))

(defun color-decode (color)
  "Decode RGB components from a long for palette usage.

Compatible with CD library definition."
  (cffi:with-foreign-objects
      ((red-ptr :unsigned-char)
       (green-ptr :unsigned-char)
       (blue-ptr :unsigned-char))
    (im-cffi::%im-color-decode red-ptr green-ptr blue-ptr color)
    (values (cffi:mem-ref red-ptr :unsigned-char)
            (cffi:mem-ref green-ptr :unsigned-char)
            (cffi:mem-ref blue-ptr :unsigned-char))))

(defalias color-mode-space-name #'im-cffi::%im-color-mode-space-name)
(defalias color-mode-to-bitmap #'im-cffi::%im-color-mode-space-name)
(defalias color-mode-bitmap-p #'im-cffi::%im-color-mode-is-bitmap)

(defalias data-type-size #'im-cffi::%im-data-type-size)
(defalias data-type-name #'im-cffi::%im-data-type-name)
(defalias data-type-int-max #'im-cffi::%im-data-type-int-max)
(defalias data-type-int-min #'im-cffi::%im-data-type-int-min)

(defun color-mode-space (color-mode)
  (cffi:foreign-enum-keyword 'im-cffi::color-space (logand color-mode #xff)))

(defun color-mode-config (color-mode)
  (cffi:foreign-bitfield-symbols 'im-cffi::color-mode-config (logand color-mode #xff00)))

(defun color-space= (color-space1 color-space2)
  (= color-space1 color-space2))

(defun color-mode= (color-mode1 color-mode2)
  (color-space= (color-mode-space color-mode1)
                (color-mode-space color-mode2)))

(defun color-mode-alpha-p (color-mode)
  (zerop (lognand color-mode (cffi:foreign-bitfield-value 'im-cffi::color-mode-config :color-mode-config-alpha))))

(defun color-mode-packed-p (color-mode)
  (zerop (lognand color-mode (cffi:foreign-bitfield-value 'im-cffi::color-mode-config :color-mode-config-packed))))

(defun color-mode-top-down-p (color-mode)
  (zerop (lognand color-mode (cffi:foreign-bitfield-value 'im-cffi::color-mode-config :color-mode-config-topdown))))

(defconstant +maximum-depth+ 5)
