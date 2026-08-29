;;;; src/ffi/im-convert.lisp — DRAFTED by tools/gen-bindings.lisp.
;;;;
;;;; Source: im_convert.h
;;;; Hand corrections below this line are expected and are kept;
;;;; re-run the generator into a clean tree and diff.

(in-package #:im.ffi)

;;; imCastMode
(cffi:defcenum cast-mode
  :cast-mode-minmax
  :cast-mode-fixed
  :cast-mode-direct
  :cast-mode-user)

;;; imGammaFactor
(cffi:defcenum gamma-factor
  (:gamma-factor-linear 0)
  (:gamma-factor-loglite -10)
  (:gamma-factor-logheavy -1000)
  (:gamma-factor-explite 2)
  (:gamma-factor-expheavy 7))

;;; imComplex2Real
(cffi:defcenum complex2-real
  :complex2-real-real
  :complex2-real-imag
  :complex2-real-mag
  :complex2-real-phase)

(cffi:defcfun ("imConvertDataType" %im-convert-data-type) :int
  "Changes the image data type, using a complex2real conversion, a gamma
factor, and an absolute mode (modulus). When demoting the data type the
function will scan source for min/max values or use fixed values
(cast_mode) to scale the result according to the target range. Except
complex to real that will use only the complex2real conversion. Images
must be of the same size and color mode. If data type is the same nothing
is done. Returns IM_ERR_NONE, IM_ERR_MEM, IM_ERR_DATA or IM_ERR_COUNTER,
see also imErrorCodes. See also imDataType, datatypeutl, imComplex2Real,
imGammaFactor and imCastMode."
  (src-image im-image)
  (dst-image im-image)
  (cpx2real :int)
  (gamma :double)
  (absolute :int)
  (cast-mode :int))

(cffi:defcfun ("imConvertColorSpace" %im-convert-color-space) :int
  "Converts one color space to another. Images must be of the same size and
data type. If color mode is the same nothing is done. CMYK can be
converted to RGB only, and it is a very simple conversion. All colors can
be converted to Binary, the non zero gray values are converted to 1. RGB
to Map uses the median cut implementation from the free IJG JPEG software,
copyright Thomas G. Lane. Alpha channel is considered and Transparency*
attributes are converted to alpha channel. All other color space
conversions assume sRGB and CIE definitions, see color. Returns
IM_ERR_NONE, IM_ERR_DATA or IM_ERR_COUNTER, see also imErrorCodes. See
also imColorSpace, imColorModeConfig and colormodeutl."
  (src-image im-image)
  (dst-image im-image))

(cffi:defcfun ("imConvertToBitmap" %im-convert-to-bitmap) :int
  "Converts the image to its bitmap equivalent, uses imConvertColorSpace and
imConvertDataType. Returns IM_ERR_NONE, IM_ERR_MEM, IM_ERR_DATA or
IM_ERR_COUNTER, see also imErrorCodes. See also imImageIsBitmap,
imComplex2Real, imGammaFactor and imCastMode. The function
im.ConvertToBitmapNew uses the default conversion result from
imColorModeToBitmap if color_space is nil."
  (src-image im-image)
  (dst-image im-image)
  (cpx2real :int)
  (gamma :double)
  (absolute :int)
  (cast-mode :int))

(cffi:defcfun ("imImageGetOpenGLData" %im-image-get-open-gl-data) :pointer
  "Returns an OpenGL compatible data buffer. Also returns the correspondent
pixel format. The memory allocated is stored in the attribute \"GLDATA\"
with BYTE type. And it will exists while the image exists. It can be
cleared by setting the attribute to NULL. MAP images are converted to RGB,
and BINARY images are converted to GRAY. Alpha channel is considered and
Transparency* attributes are converted to alpha channel. So calculate
depth from glformat, not from image depth."
  (image im-image)
  (glformat :pointer))

(cffi:defcfun ("imImageCreateFromOpenGLData" %im-image-create-from-open-gl-data) im-image
  "Creates an image from an OpenGL data."
  (width :int)
  (height :int)
  (glformat :int)
  (gldata :pointer))

(cffi:defcfun ("imConvertPacking" %im-convert-packing) :void
  "Changes the packing of the data buffer. Both must have the same width,
height and data_type. It can be used to copy data even if depth=1. Unsed
in OpenGL data conversions."
  (src-data :pointer)
  (dst-data :pointer)
  (width :int)
  (height :int)
  (src-depth :int)
  (dst-depth :int)
  (data-type :int)
  (src-is-packed :int))

(cffi:defcfun ("imConvertMapToRGB" %im-convert-map-to-rgb) :void
  "Changes in-place a MAP data into a RGB data. The data must have room for
the RGB image. depth can be 3 or 4. count=width*height. Unsed in OpenGL
data conversions."
  (data :pointer)
  (count :int)
  (depth :int)
  (packed :int)
  (palette :pointer)
  (palette-count :int))

(cffi:defcfun ("imConvertRGB2Map" %im-convert-rgb2-map) :int
  (width :int)
  (height :int)
  (red :pointer)
  (green :pointer)
  (blue :pointer)
  (map :pointer)
  (palette :pointer)
  (palette-count :pointer))

(cffi:defcfun ("imConvertRGB2MapCounter" %im-convert-rgb2-map-counter) :int
  (width :int)
  (height :int)
  (red :pointer)
  (green :pointer)
  (blue :pointer)
  (map :pointer)
  (palette :pointer)
  (palette-count :pointer)
  (counter :int))
