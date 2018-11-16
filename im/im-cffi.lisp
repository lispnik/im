(defpackage #:im-cffi
  (:use #:common-lisp)
  (:export #:im-image))

(in-package #:im-cffi)

(cffi:define-foreign-library lib-im
  (:unix "libim.so")
  (:windows "im.dll")
  (t (:default "im")))

(cffi:use-foreign-library lib-im)

;;; im_lib.h

(cffi:defcfun (%im-version "imVersion") :string)
(cffi:defcfun (%im-version-date "imVersionDate") :string)
(cffi:defcfun (%im-version-number "imVersionNumber") :int)

;;; im.h

(cffi:defcenum data-type
  data-type-byte
  data-type-short
  data-type-ushort
  data-type-int
  data-type-float
  data-type-double
  data-type-cfloat
  data-type-cdouble)

(cffi:defcenum color-space
  color-space-rgb
  color-space-map
  color-space-gray
  color-space-binary
  color-space-cmyk
  color-space-ycbcr
  color-space-lab
  color-space-luv
  color-space-xyz)

(cffi:defbitfield color-mode-config
  (color-mode-config-alpha #x100)
  (color-mode-config-packed #x200)
  (color-mode-config-topdown #x400))

(cffi:defcenum error-code
  error-code-none
  error-code-open
  error-code-access
  error-code-format
  error-code-data
  error-code-compress
  error-code-mem
  error-code-counter)

(cffi:defctype im-file :pointer)

(cffi:defcfun (%im-file-open "imFileOpen") im-file
  (filename :string)
  (error-ptr :pointer))

(cffi:defcfun (%im-file-open-as "imFileOpenAs") im-file
  (filename :string)
  (format :string)
  (error-ptr :pointer))

(cffi:defcfun (%im-file-new "imFileNew") im-file
  (filename :string)
  (format :string)
  (error-ptr :pointer))

(cffi:defcfun (%im-file-close "imFileClose") :void
  (im-file im-file))

(cffi:defcfun (%im-file-handle "imFileHandle") :pointer
  (im-file im-file)
  (index :int))

;;; NOTE this is a rather dangerous function to call. Instead of
;;; exposing this, expose the FileFormat, FileCompression and
;;; FileImageCount attributes of imFile instead.
(cffi:defcfun (%im-file-get-info "imFileGetInfo") :void
  (im-file im-file)
  (format-ptr :pointer)
  (compression-ptr :pointer)
  (image-count :pointer))

(cffi:defcfun (%im-file-set-info "imFileSetInfo") :void
  (im-file im-file)
  (compression :string))

(cffi:defcfun (%im-file-set-attribute "imFileSetAttribute") :void
  (im-file im-file)
  (attribute :string)
  (data-type data-type)
  (count :int)
  (data :pointer))

(cffi:defcfun (%im-file-set-attribute-integer "imFileSetAttribInteger") :void
  (im-file im-file)
  (attribute :string)
  (data-type data-type)
  (value :int))

(cffi:defcfun (%im-file-set-attribute-real "imFileSetAttribReal") :void
  (im-file im-file)
  (attribute :string)
  (data-type data-type)
  (value :double))

(cffi:defcfun (%im-file-set-attribute-string "imFileSetAttribString") :void
  (im-file im-file)
  (attribute :string)
  (data-type data-type)
  (value :string))

(cffi:defcfun (%im-file-get-attribute "imFileGetAttribute") :pointer
  (im-file im-file)
  (attribute :string)
  (data-type-ptr :pointer)
  (count-ptr :pointer))

(cffi:defcfun (%im-file-get-attribute-integer "imFileGetAttribInteger") :int
  (im-file im-file)
  (attribute :string)
  (index :int))

(cffi:defcfun (%im-file-get-attribute-real "imFileGetAttribReal") :double
  (im-file im-file)
  (attribute :string)
  (index :int))

(cffi:defcfun (%im-file-get-attribute-string "imFileGetAttribString") :string
  (im-file im-file)
  (attribute :string))

(cffi:defcfun (%im-file-get-attribute-list "imFileGetAttributeList") :void
  (im-file im-file)
  (attrib-ptr :pointer)
  (attrib-count-ptr :pointer))

(cffi:defcfun (%im-file-get-palette "imFileGetPalette") :void
  (im-file im-file)
  (palette-ptr :pointer)
  (palette-count-ptr :pointer))

(cffi:defcfun (%im-file-set-palette "imFileSetPalette") :void
  (im-file im-file)
  (palette-ptr :pointer)
  (palette-count :int))

(cffi:defcfun (%im-file-read-image-info "imFileReadImageInfo") :void
  (im-file im-file)
  (index :int)
  (width-ptr :pointer)
  (height-ptr :pointer)
  (file-color-mode-ptr :pointer)
  (file-data-type-ptr :pointer))

(cffi:defcfun (%im-file-write-image-info "imFileWriteImageInfo") :void
  (im-file im-file)
  (index :int)
  (width :int)
  (height :int)
  (user-color-mode :int)
  (user-data-type :int))

(cffi:defcfun (%im-file-read-image-data "imFileReadImageData") :int
  (im-file im-file)
  (data-ptr :pointer)
  (convert-to-bitmap :boolean)
  (color-mode-flags :int))

(cffi:defcfun (%im-file-write-image-data "imFileWriteImageData") :int
  (im-file im-file)
  (data-ptr :pointer))

(cffi:defcfun (%im-format-register-internal "imFormatRegisterInternal") :void)
(cffi:defcfun (%im-format-remove-all "imFormatRemoveAll") :void)

(cffi:defcfun (%im-format-list "imFormatList") :void
  (format-list-ptr :pointer)
  (format-count-ptr :pointer))

(cffi:defcfun (%im-format-info "imFormatInfo") :int
  (format :string)
  (description :pointer)                ;NOTE documented as 50 chars max
  (extensions :pointer)                 ;NOTE documented as 50 chars max, e.g. "*.tif;*.tiff;"
  (can-sequence :pointer))

(cffi:defcfun (%im-format-info-extra "imFormatInfoExtra") :int
  (format :string)
  (extra :pointer))                     ;NOTE documented as 50 chars max

(cffi:defcfun (%im-format-compressions "imFormatCompressions") :int
  (format :string)
  (compressions-ptr :pointer)		;NOTE documented as 50 compressions max (char* comp[50])
  (compressions-count :pointer)
  (color-mode :int)
  (data-type data-type))

(cffi:defcfun (%im-format-can-write-image "imFormatCanWriteImage") :boolean
  (format :string)
  (compression :string)
  (color-mode :int)
  (data-type data-type))

;;; im_raw.h

(cffi:defcfun (%im-file-open-raw "imFileOpenRaw") im-file
  (filename :string)
  (error-ptr :pointer))

(cffi:defcfun (%im-file-open-raw "imFileNewRaw") im-file
  (filename :string)
  (error-ptr :pointer))

;;; im_format_all.h

#+windows (cffi:defcfun (%im-format-register-avi "imFormatRegisterAVI") :void)

;;; im_format_ecw.h

(cffi:defcfun (%im-format-register-ecw "imFormatRegisterECW") :void)

;;; im_format.h

;;; im_format_raw.h

;;; im_format_wmv.h

#+windows (cffi:defcfun (%im-format-register-wmv "imFormatRegisterWMV") :void)

;;; im_image.h

(cffi:defctype im-image :pointer)

(cffi:defcfun (%im-file-image-save "imFileImageSave") :int
  (filename :string)
  (format :string)
  (im-image im-image))
