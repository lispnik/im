;;;; src/ffi/im-util.lisp — DRAFTED by tools/gen-bindings.lisp.
;;;;
;;;; Source: im_util.h
;;;; Hand corrections below this line are expected and are kept;
;;;; re-run the generator into a clean tree and diff.

(in-package #:im.ffi)

;;; imByteOrder
(cffi:defcenum byte-order
  :byte-order-littleendian
  :byte-order-bigendian)

(cffi:defcfun ("imStrEqual" %im-str-equal) :int
  "Check if the two strings are equal."
  (str1 :string)
  (str2 :string))

(cffi:defcfun ("imStrNLen" %im-str-nlen) :int
  "Calculate the size of the string but limited to max_len."
  (str :string)
  (max-len :int))

(cffi:defcfun ("imStrCheck" %im-str-check) :int
  "Check if the data is a string."
  (data :pointer)
  (count :int))

(cffi:defcfun ("imImageDataSize" %im-image-data-size) :int
  "Returns the size of the data buffer."
  (width :int)
  (height :int)
  (color-mode :int)
  (data-type :int))

(cffi:defcfun ("imImageLineSize" %im-image-line-size) :int
  "Returns the size of one line of the data buffer. This depends if the
components are packed. If packed includes all components, if not includes
only one."
  (width :int)
  (color-mode :int)
  (data-type :int))

(cffi:defcfun ("imImageLineCount" %im-image-line-count) :int
  "Returns the number of elements of one line of the data buffer. This
depends if the components are packed. If packed includes all components,
if not includes only one."
  (width :int)
  (color-mode :int))

(cffi:defcfun ("imImageCheckFormat" %im-image-check-format) :int
  "Check if the combination color_mode+data_type is valid."
  (color-mode :int)
  (data-type :int))

(cffi:defcfun ("imColorEncode" %im-color-encode) :long
  "Encode RGB components in a long for palette usage. \"long\" definition is
compatible with the CD library definition."
  (red :unsigned-char)
  (green :unsigned-char)
  (blue :unsigned-char))

(cffi:defcfun ("imColorDecode" %im-color-decode) :void
  "Decode RGB components from a long for palette usage. \"long\" definition
is compatible with the CD library definition."
  (red :pointer)
  (green :pointer)
  (blue :pointer)
  (color :long))

(cffi:defcfun ("imColorModeSpaceName" %im-color-mode-space-name) :string
  "Returns the color mode name."
  (color-mode :int))

(cffi:defcfun ("imColorModeComponentName" %im-color-mode-component-name) :string
  "Returns the color mode space component name."
  (color-space :int)
  (component :int))

(cffi:defcfun ("imColorModeDepth" %im-color-mode-depth) :int
  "Returns the number of components of the color space including alpha."
  (color-mode :int))

(cffi:defcfun ("imColorModeToBitmap" %im-color-mode-to-bitmap) :int
  "Returns the color space of the equivalent display bitmap image. Original
packing and alpha are ignored. Returns IM_RGB, IM_GRAY, IM_MAP or
IM_BINARY."
  (color-mode :int))

(cffi:defcfun ("imColorModeIsBitmap" %im-color-mode-is-bitmap) :int
  "Check if the color mode and data_type defines a display bitmap image."
  (color-mode :int)
  (data-type :int))

(cffi:defcfun ("imDataTypeSize" %im-data-type-size) :int
  "Returns the size in bytes of a specified numeric data type."
  (data-type :int))

(cffi:defcfun ("imDataTypeName" %im-data-type-name) :string
  "Returns the numeric data type name given its identifier."
  (data-type :int))

(cffi:defcfun ("imDataTypeIntMax" %im-data-type-int-max) :unsigned-long
  "Returns the maximum value of an integer data type. For floating point
returns 0."
  (data-type :int))

(cffi:defcfun ("imDataTypeIntMin" %im-data-type-int-min) :long
  "Returns the minimum value of an integer data type. For floating point
returns 0."
  (data-type :int))

(cffi:defcfun ("imBinCPUByteOrder" %im-bin-cpu-byte-order) :int
  "Returns the current CPU byte order.")

(cffi:defcfun ("imBinSwapBytes" %im-bin-swap-bytes) :void
  "Changes the byte order of an array of \\a count values, each \\a size
bytes. \\a size is the width of one scalar value: 1 (a no-op), 2, 4 or 8.
It also accepts 16, the width of IM_CDOUBLE, which is swapped as two
independent 8-byte halves. Complex values must be passed as their real
components -- the size of one component and twice the count -- because
element size alone cannot tell an IM_CFLOAT (two 4-byte floats) from a
double. Passing imDataTypeSize(IM_CFLOAT) directly byte-swaps correctly
but transposes the real and imaginary parts. A NULL \\a data, a \\a count
of zero or less, and any other \\a size leave the data untouched."
  (data :pointer)
  (count :int)
  (size :int))

(cffi:defcfun ("imBinSwapBytes2" %im-bin-swap-bytes2) :void
  "Changes the byte order of an array of 2 byte values."
  (data :pointer)
  (count :int))

(cffi:defcfun ("imBinSwapBytes4" %im-bin-swap-bytes4) :void
  "Inverts the byte order of the 4 byte values"
  (data :pointer)
  (count :int))

(cffi:defcfun ("imBinSwapBytes8" %im-bin-swap-bytes8) :void
  "Inverts the byte order of the 8 byte values"
  (data :pointer)
  (count :int))

(cffi:defcfun ("imCompressDataZ" %im-compress-data-z) :int
  "Compresses the data using the ZLIB Deflate compression. The destination
buffer must be at least 0.1% larger than source_size plus 12 bytes. It
compresses raw byte data. zip_quality can be 1 to 9. Returns the size of
the compressed buffer or zero if failed."
  (src-data :pointer)
  (src-size :int)
  (dst-data :pointer)
  (dst-size :int)
  (zip-quality :int))

(cffi:defcfun ("imCompressDataUnZ" %im-compress-data-un-z) :int
  "Uncompresses the data compressed with the ZLIB Deflate compression.
Returns zero if failed."
  (src-data :pointer)
  (src-size :int)
  (dst-data :pointer)
  (dst-size :int))

(cffi:defcfun ("imCompressDataLZF" %im-compress-data-lzf) :int
  "Compresses the data using the libLZF compression. Returns the size of the
compressed buffer or zero if failed."
  (src-data :pointer)
  (src-size :int)
  (dst-data :pointer)
  (dst-size :int))

(cffi:defcfun ("imCompressDataUnLZF" %im-compress-data-un-lzf) :int
  "Uncompresses the data compressed with the libLZF compression. Returns zero
if failed."
  (src-data :pointer)
  (src-size :int)
  (dst-data :pointer)
  (dst-size :int))

(cffi:defcfun ("imCompressDataLZ4" %im-compress-data-lz4) :int
  "Compresses the data using the libLZ4 compression. (Since 3.15) Returns the
size of the compressed buffer or zero if failed. Available in a separate
library called \"im_lz4\"."
  (src-data :pointer)
  (src-size :int)
  (dst-data :pointer)
  (dst-size :int))

(cffi:defcfun ("imCompressDataUnLZ4" %im-compress-data-un-lz4) :int
  "Uncompresses the data compressed with the libLZ4 compression. (Since 3.15)
Returns zero if failed. Available in a separate library called \"im_lz4\"."
  (src-data :pointer)
  (src-size :int)
  (dst-data :pointer)
  (dst-size :int))
