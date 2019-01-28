(defpackage #:im-cffi
  (:use #:common-lisp)
  (:import-from #:fpw #:define-foreign-pointer-wrapper)
  (:export #:im-file
	   #:im-image
	   #:make-im-file
	   #:make-im-image))

(in-package #:im-cffi)

(cffi:define-foreign-library lib-im
  (:unix "libim.so")
  (:windows "im.dll")
  (t (:default "im")))

(cffi:use-foreign-library lib-im)

(cffi:defctype palette (:pointer :long))

;;; im_lib.h

(cffi:defcfun (%im-version "imVersion") :string)
(cffi:defcfun (%im-version-date "imVersionDate") :string)
(cffi:defcfun (%im-version-number "imVersionNumber") :int)

;;; im.h

(cffi:defcenum data-type
  :data-type-byte
  :data-type-short
  :data-type-ushort
  :data-type-int
  :data-type-float
  :data-type-double
  :data-type-cfloat
  :data-type-cdouble)

(cffi:defcenum color-space
  :color-space-rgb
  :color-space-map
  :color-space-gray
  :color-space-binary
  :color-space-cmyk
  :color-space-ycbcr
  :color-space-lab
  :color-space-luv
  :color-space-xyz)

(cffi:defbitfield color-mode-config
  (:color-mode-config-default 0)        ;unpacked, no alpha, bottom-up
  (:color-mode-config-alpha #x100)
  (:color-mode-config-packed #x200)
  (:color-mode-config-topdown #x400))

(cffi:defcenum error-code
  :error-code-none
  :error-code-open
  :error-code-access
  :error-code-format
  :error-code-data
  :error-code-compress
  :error-code-mem
  :error-code-counter)

(define-foreign-pointer-wrapper im-file)
(define-foreign-pointer-wrapper im-image)

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

(cffi:defcfun (%im-file-get-info "imFileGetInfo") :void
  (im-file im-file)
  (format-ptr :pointer)
  (compression-ptr :pointer)
  (image-count :pointer))

(cffi:defcfun (%im-file-set-info "imFileSetInfo") :void
  (im-file im-file)
  (compression :pointer))

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
  (palette-ptr palette)
  (palette-count-ptr :pointer))

(cffi:defcfun (%im-file-set-palette "imFileSetPalette") :void
  (im-file im-file)
  (palette-ptr palette)
  (palette-count :int))

(cffi:defcfun (%im-file-read-image-info "imFileReadImageInfo") error-code
  (im-file im-file)
  (index :int)
  (width-ptr :pointer)
  (height-ptr :pointer)
  (file-color-mode-ptr :pointer)
  (file-data-type-ptr :pointer))

(cffi:defcfun (%im-file-write-image-info "imFileWriteImageInfo") error-code
  (im-file im-file)
  (width :int)
  (height :int)
  (color-mode :int)
  (data-type data-type))

(cffi:defcfun (%im-file-read-image-data "imFileReadImageData") error-code
  (im-file im-file)
  (data-ptr :pointer)
  (convert-to-bitmap :boolean)
  (color-mode-flags :int))

(cffi:defcfun (%im-file-write-image-data "imFileWriteImageData") error-code
  (im-file im-file)
  (data-ptr :pointer))

(cffi:defcfun (%im-format-register-internal "imFormatRegisterInternal") :void)
(cffi:defcfun (%im-format-remove-all "imFormatRemoveAll") :void)

(cffi:defcfun (%im-format-list "imFormatList") :void
  (format-list-ptr :pointer)
  (format-count-ptr :pointer))

(cffi:defcfun (%im-format-info "imFormatInfo") error-code
  (format :string)
  (description :pointer)                ;NOTE documented as 50 chars max
  (extensions :pointer)                 ;NOTE documented as 50 chars max, e.g. "*.tif;*.tiff;"
  (can-sequence :pointer))

(cffi:defcfun (%im-format-info-extra "imFormatInfoExtra") error-code
  (format :string)
  (extra :pointer))                     ;NOTE documented as 50 chars max

(cffi:defcfun (%im-format-compressions "imFormatCompressions") error-code
  (format :string)
  (compressions-ptr :pointer)		;NOTE documented as 50 compressions max (char* comp[50])
  (compressions-count :pointer)
  (color-mode :int)
  (data-type :int))

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

#+windows
(cffi:defcfun (%im-format-register-avi "imFormatRegisterAVI") :void)

;;; im_format_ecw.h

(cffi:defcfun (%im-format-register-ecw "imFormatRegisterECW") :void)

;;; im_format.h

;;; im_format_raw.h

;;; im_format_wmv.h

#+windows
(cffi:defcfun (%im-format-register-wmv "imFormatRegisterWMV") :void)

;;; im_image.h

(cffi:defcstruct im-image-struct
  (width :int)
  (height :int)
  (color-space color-space)
  (data-type data-type)
  (has-alpha-p :boolean)
  (depth :int)
  (plane-size :int)
  (size :int)
  (count :int)
  (data (:pointer :pointer))
  (palette (:pointer :long))
  (palette-count :int)
  (attribute-table :pointer))

(cffi:defcfun (%im-image-create "imImageCreate") im-image
  (width :int)
  (height :int)
  (color-space color-space)
  (data-type data-type))

(cffi:defcfun (%im-image-init "imImageInit") im-image
  (width :int)
  (height :int)
  (color-space color-space)
  (data-type data-type)
  (data-buffer :pointer)
  (palette-ptr palette)
  (palette-count :int))

(cffi:defcfun (%im-image-create-based "imImageCreateBased") im-image
  (im-image im-image)
  (width :int)
  (height :int)
  (color-space :int)
  (data-type :int))

(cffi:defcfun (%im-image-destroy "imImageDestroy") :void
  (im-image im-image))

(cffi:defcfun (%im-image-add-alpha "imImageAddAlpha") :void
  (im-image im-image))

(cffi:defcfun (%im-image-set-alpha "imImageSetAlpha") :void
  (im-image im-image)
  (alpha :float))

(cffi:defcfun (%im-image-remove-alpha "imImageRemoveAlpha") :void
  (im-image im-image))

(cffi:defcfun (%im-image-reshape "imImageReshape") :void
  (im-image im-image)
  (width :int)
  (height :int))

(cffi:defcfun (%im-image-copy "imImageCopy") :void
  (src-im-image im-image)
  (dst-im-image im-image))

(cffi:defcfun (%im-image-copy-data "imImageCopyData") :void
  (src-im-image im-image)
  (dst-im-image im-image))

(cffi:defcfun (%im-image-copy-attributes "imImageCopyAttributes") :void
  (src-im-image im-image)
  (dst-im-image im-image))

(cffi:defcfun (%im-image-merge-attributes "imImageMergeAttributes") :void
  (src-im-image im-image)
  (dst-im-image im-image))

(cffi:defcfun (%im-image-copy-plane "imImageCopyPlane") :void
  (src-im-image im-image)
  (src-plane :int)
  (dst-im-image im-image)
  (dst-plane :int))

(cffi:defcfun (%im-image-duplicate "imImageDuplicate") im-image
  (im-image im-image))

(cffi:defcfun (%im-image-clone "imImageClone") im-image
  (im-image im-image))

(cffi:defcfun (%im-image-set-attribute "imImageSetAttribute") :void
  (im-image im-image)
  (attribute :string)
  (data-type data-type)
  (count :int)
  (data :pointer))

(cffi:defcfun (%im-image-set-attrib-integer "imImageSetAttribInteger") :void
  (im-image im-image)
  (attribute :string)
  (data-type data-type)
  (value :int))

(cffi:defcfun (%im-image-set-attrib-real "imImageSetAttribReal") :void
  (im-image im-image)
  (attribute :string)
  (data-type data-type)
  (value :double))

(cffi:defcfun (%im-image-set-attrib-string "imImageSetAttribString") :void
  (im-image im-image)
  (attribute :string)
  (value :string))

(cffi:defcfun (%im-image-get-attribute "imImageGetAttribute") :pointer
  (im-image im-image)
  (attribute :string)
  (data-type data-type)
  (count (:pointer :int)))

(cffi:defcfun (%im-image-get-attrib-integer "imImageGetAttribInteger") :pointer
  (im-image im-image)
  (attribute :string)
  (index :int))

(cffi:defcfun (%im-image-get-attrib-real "imImageGetAttribReal") :double
  (im-image im-image)
  (attribute :string)
  (index :int))

(cffi:defcfun (%im-image-get-attrib-string "imImageGetAttribString") :string
  (im-image im-image)
  (attribute :string))

(cffi:defcfun (%im-image-get-attribute-list "imImageGetAttributeList") :void
  (im-image im-image)
  (attrib (:pointer :string))
  (count (:pointer :int)))

(cffi:defcfun (%im-image-clear "imImageClear") :void
  (im-image im-image))

(cffi:defcfun (%im-image-is-bitmap "imImageIsBitmap") :boolean
  (im-image im-image))

(cffi:defcfun (%im-image-set-palette "imImageSetPalette") :void
  (im-image im-image)
  (palette palette)
  (palette-count :int))

(cffi:defcfun (%im-image-match-size "imImageMatchSize") :boolean
  (im-image1 im-image)
  (im-image2 im-image))

(cffi:defcfun (%im-image-match-color "imImageMatchColor") :boolean
  (im-image1 im-image)
  (im-image2 im-image))

(cffi:defcfun (%im-image-match-data-type "imImageMatchDataType") :boolean
  (im-image1 im-image)
  (im-image2 im-image))

(cffi:defcfun (%im-image-match-color-space "imImageMatchColorSpace") :boolean
  (im-image1 im-image)
  (im-image2 im-image))

(cffi:defcfun (%im-image-match "imImageMatch") :boolean
  (im-image1 im-image)
  (im-image2 im-image))

(cffi:defcfun (%im-image-set-map "imImageSetMap") :void
  (im-image im-image))

(cffi:defcfun (%im-image-set-binary "imImageSetBinary") :void
  (im-image im-image))

(cffi:defcfun (%im-image-set-gray "imImageSetGray") :void
  (im-image im-image))

(cffi:defcfun (%im-image-make-binary "imImageMakeBinary") :void
  (im-image im-image))

(cffi:defcfun (%im-image-make-gray "imImageMakeGray") :void
  (im-image im-image))

(cffi:defcfun (%im-file-load-image "imFileLoadImage") im-image
  (im-file im-file)
  (index :int)
  (error-ptr :pointer))

(cffi:defcfun (%im-file-load-image-frame "imFileLoadImageFrame") :void
  (im-file im-file)
  (index :int)
  (im-image im-image)
  (error-ptr :pointer))

(cffi:defcfun (%im-file-load-bitmap "imFileLoadBitmap") im-image
  (im-file im-file)
  (index :int)
  (error-ptr :pointer))

(cffi:defcfun (%im-file-load-image-region "imFileLoadImageRegion") im-image
  (im-file im-file)
  (index :int)
  (bitmap :boolean)
  (error-ptr :pointer)
  (xmin :int)
  (xmax :int)
  (ymin :int)
  (ymax :int)
  (width :int)
  (height :int))

(cffi:defcfun (%im-file-load-bitmap-frame "imFileLoadBitmapFrame") :void
  (im-file im-file)
  (index :int)
  (im-image im-image)
  (error-ptr :pointer))

(cffi:defcfun (%im-file-save-image "imFileSaveImage") :int
  (im-file im-file)
  (im-image im-image))

(cffi:defcfun (%im-file-image-load "imFileImageLoad") im-image
  (filename :string)
  (index :int)
  (error-ptr :pointer))

(cffi:defcfun (%im-file-image-load-bitmap "imFileImageLoadBitmap") im-image
  (filename :string)
  (index :int)
  (error-ptr :pointer))

(cffi:defcfun (%im-file-image-load-region "imFileImageLoadRegion") im-image
  (filename :string)
  (index :int)
  (bitmap :boolean)
  (error-ptr :pointer)
  (xmin :int)
  (xmax :int)
  (ymin :int)
  (ymax :int)
  (width :int)
  (height :int))

(cffi:defcfun (%im-file-image-save "imFileImageSave") :int
  (filename :string)
  (format :string)
  (im-image im-image))

;;; im_util.h

(cffi:defcfun (%im-image-data-size "imImageDataSize") :int
  (width :int)
  (height :int)
  (color-mode :int)
  (data-type data-type))

(cffi:defcfun (%im-image-line-size "imImageLineSize") :int
  (width :int)
  (color-mode :int)
  (data-type data-type))

(cffi:defcfun (%im-image-line-count "imImageLineCount") :int
  (width :int)
  (color-mode :int))

(cffi:defcfun (%im-image-check-format "imImageCheckFormat") :boolean
  (color-mode :int)
  (data-type data-type))

(cffi:defcfun (%im-color-encode "imColorEncode") :long
  (red :unsigned-char)
  (green :unsigned-char)
  (blue :unsigned-char))

(cffi:defcfun (%im-color-decode "imColorDecode") :long
  (red (:pointer :unsigned-char))
  (green (:pointer :unsigned-char))
  (blue (:pointer :unsigned-char))
  (color :long))

(cffi:defcfun (%im-color-mode-space-name "imColorModeSpaceName") :string
  (color-mode :int))

(cffi:defcfun (%im-color-mode-depth "imColorModeDepth") :int
  (color-mode :int))

(cffi:defcfun (%im-color-mode-to-bitmap "imColorModeToBitmap") :int
  (color-mode :int))

(cffi:defcfun (%im-color-mode-is-bitmap "imColorModeIsBitmap") :boolean
  (color-mode :int)
  (data-type :int))

(cffi:defcfun (%im-data-type-size "imDataTypeSize") :int
  (data-type :int))

(cffi:defcfun (%im-data-type-name "imDataTypeName") :string
  (data-type :int))

(cffi:defcfun (%im-data-type-int-max "imDataTypeIntMax") :unsigned-long
  (data-type :int))

(cffi:defcfun (%im-data-type-int-min "imDataTypeIntMin") :long
  (data-type :int))

;;; im_palette.h

(cffi:defcfun (%im-palette-new "imPaletteNew") palette
  (count :int))

(cffi:defcfun (%im-palette-release "imPaletteRelease") :void
  (palette palette))

(cffi:defcfun (%im-palette-duplicate "imPaletteDuplicate") palette
  (palette palette)
  (count :int))

(cffi:defcfun (%im-palette-find-nearest "imPaletteFindNearest") :int
  (palette palette)
  (count :int)
  (color :long))

(cffi:defcfun (%im-palette-find-color "imPaletteFindColor") :int
  (palette palette)
  (count :int)
  (color :long)
  (tol :unsigned-char))

(cffi:defcfun (%im-palette-gray "imPaletteGray") palette)
(cffi:defcfun (%im-palette-red "imPaletteRed") palette)
(cffi:defcfun (%im-palette-green "imPaletteGreen") palette)
(cffi:defcfun (%im-palette-blue "imPaletteBlue") palette)
(cffi:defcfun (%im-palette-yellow "imPaletteYellow") palette)
(cffi:defcfun (%im-palette-magenta "imPaletteMagenta") palette)
(cffi:defcfun (%im-palette-cian "imPaletteCian") palette)
(cffi:defcfun (%im-palette-rainbow "imPaletteRainbow") palette)
(cffi:defcfun (%im-palette-hues "imPaletteHues") palette)
(cffi:defcfun (%im-palette-blue-ice "imPaletteBlueIce") palette)
(cffi:defcfun (%im-palette-hot-iron "imPaletteHotIron") palette)
(cffi:defcfun (%im-palette-high-contrast "imPaletteHighContrast") palette)
(cffi:defcfun (%im-palette-linear "imPaletteLinear") palette)
(cffi:defcfun (%im-palette-uniform "imPaletteUniform") palette)

(cffi:defcfun (%im-palette-uniform-index "imPaletteUniformIndex") palette
  (color :long))

(cffi:defcfun (%im-palette-uniform-index-halftoned "imPaletteUniformIndexHalftoned") palette
  (color :long)
  (x :int)
  (y :int))

;;; im_convert.h

(cffi:defcenum complex-to-real
  :complex-to-real
  :complex-to-imaginary
  :complex-to-magnitude
  :complex-to-phase)

(cffi:defcenum cast-mode
  :cast-mode-minimax
  :cast-mode-fixed
  :cast-mode-direct
  :cast-mode-user)

(cffi:defcfun (%im-convert-data-type "imConvertDataType") error-code
  (src-im-image im-image)
  (dst-im-image im-image)
  (complex-to-real complex-to-real)
  (gamma :float)
  (absolute-p :boolean)
  (cast-mode cast-mode))

(cffi:defcfun (%im-convert-color-space "imConvertColorSpace") error-code
  (src-im-image im-image)
  (dst-im-image im-image))

(cffi:defcfun (%im-convert-to-bitmap "imConvertToBitmap") error-code
  (src-im-image im-image)
  (dst-im-image im-image)
  (complex-to-real complex-to-real)
  (gamma :float)
  (absolute-p :boolean)
  (cast-mode cast-mode))

(cffi:defcfun (%im-convert-color-space "imConvertColorSpace") error-code
  (src-im-image im-image)
  (dst-im-image im-image))

(cffi:defcfun (%im-convert-packing "imConvertPacking") :void
  (src-data im-image)
  (dst-data im-image)
  (width :int)
  (height :int)
  (src-depth :int)
  (dst-depth :int)
  (data-type data-type)
  (src-is-packed-p :boolean))

(cffi:defcfun (%im-convert-map-to-rgb "imConvertMapToRGB") :void
  (data :pointer)
  (count :int)
  (depth :int)
  (packed :int)
  (palette palette)
  (palette-count :int))

;;; im_util.h

(cffi:defcfun (%im-compress-data-z "imCompressDataZ") :int
  (src-data-ptr :pointer)
  (src-size :int)
  (dst-data-ptr :pointer)
  (dst-size :int)
  (quality :int))

(cffi:defcfun (%im-compress-data-un-z "imCompressDataUnZ") :int
  (src-data-ptr :pointer)
  (src-size :int)
  (dst-data-ptr :pointer)
  (dst-size :int))

(cffi:defcfun (%im-compress-data-lzf "imCompressDataLZF") :int
  (src-data-ptr :pointer)
  (src-size :int)
  (dst-data-ptr :pointer)
  (dst-size :int))

(cffi:defcfun (%im-compress-data-un-lzf "imCompressDataUnLZF") :int
  (src-data-ptr :pointer)
  (src-size :int)
  (dst-data-ptr :pointer)
  (dst-size :int))

(cffi:defcfun (%im-compress-data-un-lzo "imCompressDataLZO") :int
  (src-data-ptr :pointer)
  (src-size :int)
  (dst-data-ptr :pointer)
  (dst-size :int))

(cffi:defcfun (%im-compress-data-un-lzo "imCompressDataUnLZO") :int
  (src-data-ptr :pointer)
  (src-size :int)
  (dst-data-ptr :pointer)
  (dst-size :int))

;;; im_kernel.h

(cffi:defcfun (%im-kernel-sobel "imKernelSobel") im-image)
(cffi:defcfun (%im-kernel-prewitt "imKernelPrewitt") im-image)
(cffi:defcfun (%im-kernel-kirsh "imKernelKirsh") im-image)
(cffi:defcfun (%im-kernel-laplacian-4 "imKernelLaplacian4") im-image)
(cffi:defcfun (%im-kernel-laplacian-8 "imKernelLaplacian8") im-image)
(cffi:defcfun (%im-kernel-laplacian-5x5 "imKernelLaplacian5x5") im-image)
(cffi:defcfun (%im-kernel-laplacian-7x7 "imKernelLaplacian7x7") im-image)
(cffi:defcfun (%im-kernel-gradian-3x3 "imKernelGradian3x3") im-image)
(cffi:defcfun (%im-kernel-gradian-7x7 "imKernelGradian7x7") im-image)
(cffi:defcfun (%im-kernel-sculpt "imKernelSculpt") im-image)
(cffi:defcfun (%im-kernel-mean-3x3 "imKernelMean3x3") im-image)
(cffi:defcfun (%im-kernel-mean-5x5 "imKernelMean5x5") im-image)
(cffi:defcfun (%im-kernel-mean-7x7 "imKernelMean7x7") im-image)
(cffi:defcfun (%im-kernel-circular-mean-5x5 "imKernelCircularMean5x5") im-image)
(cffi:defcfun (%im-kernel-circular-mean-7x7 "imKernelCircularMean7x7") im-image)
(cffi:defcfun (%im-kernel-guassian-3x3 "imKernelGausian3x3") im-image)
(cffi:defcfun (%im-kernel-guassian-5x5 "imKernelGausian5x5") im-image)
(cffi:defcfun (%im-kernel-barlett-5x5 "imKernelBarlett5x5") im-image)
(cffi:defcfun (%im-kernel-top-hat-5x5 "imKernelTopHat5x5") im-image)
(cffi:defcfun (%im-kernel-top-hat-7x7 "imKernelTopHat7x7") im-image)
(cffi:defcfun (%im-kernel-enhance "imKernelEnhance") im-image)
