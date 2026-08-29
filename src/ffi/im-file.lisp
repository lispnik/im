;;;; src/ffi/im-file.lisp — DRAFTED by tools/gen-bindings.lisp.
;;;;
;;;; Source: im.h, im_raw.h
;;;; Hand corrections below this line are expected and are kept;
;;;; re-run the generator into a clean tree and diff.

(in-package #:im.ffi)

;;; imErrorCodes
(cffi:defcenum error-code
  :error-code-none
  :error-code-open
  :error-code-access
  :error-code-format
  :error-code-data
  :error-code-compress
  :error-code-mem
  :error-code-counter)

;;; imColorModeConfig
(cffi:defbitfield color-mode-config
  (:color-mode-config-alpha #x100)
  (:color-mode-config-packed #x200)
  (:color-mode-config-topdown #x400))

;;; imColorSpace
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

;;; imDataType
(cffi:defcenum data-type
  :data-type-byte
  :data-type-short
  :data-type-ushort
  :data-type-int
  :data-type-float
  :data-type-double
  :data-type-cfloat
  :data-type-cdouble)

(cffi:defcfun ("imFileOpen" %im-file-open) im-file
  "Opens the file for reading. It must exists. Also reads file header. It
will try to identify the file format. See also imErrorCodes. In Lua the IM
file metatable name is \"imFile\". When converted to a string will return
\"imFile(%p)\" where %p is replaced by the userdata address. If the file
is already closed by im.FileClose, then it will return also the suffix
\"-closed\"."
  (file-name :string)
  (error :pointer))

(cffi:defcfun ("imFileOpenAs" %im-file-open-as) im-file
  "Opens the file for reading using a specific format. It must exists. Also
reads file header. See also imErrorCodes and format."
  (file-name :string)
  (format :string)
  (error :pointer))

(cffi:defcfun ("imFileNew" %im-file-new) im-file
  "Creates a new file for writing using a specific format. If the file exists
will be replaced. It will only initialize the format driver and create the
file, no data is actually written. See also imErrorCodes and format."
  (file-name :string)
  (format :string)
  (error :pointer))

(cffi:defcfun ("imFileClose" %im-file-close) :void
  "Closes the file. In Lua if this function is not called, the file is closed
by the garbage collector."
  (ifile im-file))

(cffi:defcfun ("imFileHandle" %im-file-handle) :pointer
  "Returns an internal handle. index=0 returns always an imBinFile* handle,
but for some formats returns NULL because they do not use imBinFile (like
AVI and WMV). index=1 return an internal structure used by the format,
usually is a handle to a third party library structure. This is file
format dependent."
  (ifile im-file)
  (index :int))

(cffi:defcfun ("imFileGetInfo" %im-file-get-info) :void
  "Returns file information. image_count is the number of images in a stack
or the number of frames in a video/animation or the depth of a volume
data. compression and image_count can be NULL. These information are also
available as attributes: See also format."
  (ifile im-file)
  (format :pointer)
  (compression :pointer)
  (image-count :pointer))

(cffi:defcfun ("imFileSetInfo" %im-file-set-info) :void
  "Changes the write compression method. If the compression is not supported
will return an error code when writing. Use NULL to set the default
compression. You can use the imFileGetInfo to retrieve the actual
compression but only after imFileWriteImageInfo. Only a few formats allow
you to change the compression between frames."
  (ifile im-file)
  (compression :string))

(cffi:defcfun ("imFileSetAttribute" %im-file-set-attribute) :void
  "Changes an extended attribute. The data will be internally duplicated. If
data is NULL the attribute is removed. If data_type is BYTE then count can
be -1 to indicate a NULL terminated string. See also imDataType. If
data_type is IM_BYTE, as_string can be used as data."
  (ifile im-file)
  (attrib :string)
  (data-type :int)
  (count :int)
  (data :pointer))

(cffi:defcfun ("imFileSetAttribInteger" %im-file-set-attrib-integer) :void
  "Changes an extended attribute as an integer."
  (ifile im-file)
  (attrib :string)
  (data-type :int)
  (value :int))

(cffi:defcfun ("imFileSetAttribReal" %im-file-set-attrib-real) :void
  "Changes an extended attribute as a real."
  (ifile im-file)
  (attrib :string)
  (data-type :int)
  (value :double))

(cffi:defcfun ("imFileSetAttribString" %im-file-set-attrib-string) :void
  "Changes an extended attribute as a string."
  (ifile im-file)
  (attrib :string)
  (value :string))

(cffi:defcfun ("imFileGetAttribute" %im-file-get-attribute) :pointer
  "Returns an extended attribute. Returns NULL if not found. data_type and
count can be NULL. See also imDataType. If data_type is IM_BYTE, as_string
can be used to return a string instead of a table."
  (ifile im-file)
  (attrib :string)
  (data-type :pointer)
  (count :pointer))

(cffi:defcfun ("imFileGetAttribInteger" %im-file-get-attrib-integer) :int
  "Returns an extended attribute as an integer."
  (ifile im-file)
  (attrib :string)
  (index :int))

(cffi:defcfun ("imFileGetAttribReal" %im-file-get-attrib-real) :double
  "Returns an extended attribute as a real."
  (ifile im-file)
  (attrib :string)
  (index :int))

(cffi:defcfun ("imFileGetAttribString" %im-file-get-attrib-string) :string
  "Returns an extended attribute as a string."
  (ifile im-file)
  (attrib :string))

(cffi:defcfun ("imFileGetAttributeList" %im-file-get-attribute-list) :void
  "Returns a list of the attribute names. \"attrib\" must contain room enough
for \"attrib_count\" names. Use \"attrib=NULL\" to return only the count.
The array receives pointers to the names held inside the attribute table,
not copies, so do not free them. They are also only valid while those
entries exist: setting or removing any attribute, or destroying the owner,
can free a name a previous call handed out. Copy anything you need to keep
across a change to the attributes."
  (ifile im-file)
  (attrib :pointer)
  (attrib-count :pointer))

(cffi:defcfun ("imFileGetPalette" %im-file-get-palette) :void
  "Returns the palette if any. \"palette\" must be a 256 colors allocated
array. Returns zero in \"palette_count\" if there is no palette.
\"palette_count\" is >0 and <=256."
  (ifile im-file)
  (palette :pointer)
  (palette-count :pointer))

(cffi:defcfun ("imFileSetPalette" %im-file-set-palette) :void
  "Changes the pallete. \"palette_count\" is >0 and <=256."
  (ifile im-file)
  (palette :pointer)
  (palette-count :int))

(cffi:defcfun ("imFileReadImageInfo" %im-file-read-image-info) :int
  "Reads the image header if any and returns image information. Reads also
the extended image attributes, so other image attributes will be available
only after calling this function. Returns an error code. index specifies
the image number between 0 and image_count-1. Some drivers reads only in
sequence, so \"index\" can be ignored by the format driver. Any parameters
can be NULL. This function must be called at least once, check each format
documentation. See also imErrorCodes, imDataType, imColorSpace and
imColorModeConfig. Default index is 0."
  (ifile im-file)
  (index :int)
  (width :pointer)
  (height :pointer)
  (file-color-mode :pointer)
  (file-data-type :pointer))

(cffi:defcfun ("imFileWriteImageInfo" %im-file-write-image-info) :int
  "Writes the image header. Writes the file header at the first time it is
called. Writes also the extended image attributes. Must call
imFileSetPalette and set other attributes before calling this function. In
some formats the color space will be converted to match file format
specification. Returns an error code. This function must be called at
least once, check each format documentation. See also imErrorCodes,
imDataType, imColorSpace and imColorModeConfig."
  (ifile im-file)
  (width :int)
  (height :int)
  (user-color-mode :int)
  (user-data-type :int))

(cffi:defcfun ("imFileReadImageData" %im-file-read-image-data) :int
  "Reads the image data with or without conversion. The data can be converted
to bitmap when reading. Data type conversion to byte will always scan for
min-max then scale to 0-255, except integer values that min-max are
already between 0-255. Complex to real conversions will use the magnitude.
Color mode flags contains packed, alpha and top-bottom information. If
flag is 0 means unpacked, no alpha and bottom up. If flag is -1 the file
original flags are used. Returns an error code. See also imErrorCodes,
imDataType, imColorSpace and imColorModeConfig."
  (ifile im-file)
  (data :pointer)
  (convert2bitmap :int)
  (color-mode-flags :int))

(cffi:defcfun ("imFileWriteImageData" %im-file-write-image-data) :int
  "Writes the image data. Returns an error code."
  (ifile im-file)
  (data :pointer))

(cffi:defcfun ("imFormatRegisterInternal" %im-format-register-internal) :void
  "Registers all the internal formats. It is automatically called internally
when a format is accessed, but can be called to force the internal formats
to be registered before other formats. Notice that additional formats when
registered will be registered before the internal formats if
imFormatRegisterInternal is not called yet. To control the register order
is useful when two format drivers handle the same format. The first
registered format will always be used first.")

(cffi:defcfun ("imFormatRemoveAll" %im-format-remove-all) :void
  "Remove all registered formats. Call this if you are checking memory leaks.")

(cffi:defcfun ("imFormatList" %im-format-list) :void
  "Returns a list of the registered formats. format_list is an array of
format identifiers. Each format identifier is 10 chars max, maximum of 50
formats. You can use \"char* format_list[50]\". The array receives
pointers into storage owned by this function, not copies into buffers you
supply. So do not allocate the strings before calling and do not free them
afterwards -- freeing what comes back corrupts the heap. That storage is
also reused, so the strings are only valid until the next call to
imFormatList; copy anything you need to keep."
  (format-list :pointer)
  (format-count :pointer))

(cffi:defcfun ("imFormatInfo" %im-format-info) :int
  "Returns the format description. Format description is 50 chars max.
Extensions are separated like \"*.tif;*.tiff;\", 50 chars max. Returns an
error code. The parameters can be NULL, except format. See also format."
  (format :string)
  (desc :pointer)
  (ext :pointer)
  (can-sequence :pointer))

(cffi:defcfun ("imFormatInfoExtra" %im-format-info-extra) :int
  "Returns the format information of the third party library used to support
the format. Format extra is 50 chars max. Returns an error code. See also
format."
  (format :string)
  (extra :pointer))

(cffi:defcfun ("imFormatCompressions" %im-format-compressions) :int
  "Returns the format compressions. Compressions are 20 chars max each,
maximum of 50 compressions. You can use \"char* comp[50]\". As with
imFormatList, the array receives pointers into storage owned by this
function rather than copies into buffers you supply: do not allocate the
strings and do not free them, and copy them if you need them to outlive
the next call. color_mode and data_type are optional, use -1 to ignore
them. If you use them they will select only the allowed compressions
checked like in imFormatCanWriteImage. Returns an error code. See also
format, imErrorCodes, imDataType, imColorSpace and imColorModeConfig."
  (format :string)
  (comp :pointer)
  (comp-count :pointer)
  (color-mode :int)
  (data-type :int))

(cffi:defcfun ("imFormatCanWriteImage" %im-format-can-write-image) :int
  "Checks if the format support the given image class at the given
compression. Returns an error code. See also format, imErrorCodes,
imDataType, imColorSpace and imColorModeConfig."
  (format :string)
  (compression :string)
  (color-mode :int)
  (data-type :int))

(cffi:defcfun ("imFileOpenRaw" %im-file-open-raw) im-file
  "Opens a RAW image file. See also imErrorCodes."
  (file-name :string)
  (error :pointer))

(cffi:defcfun ("imFileNewRaw" %im-file-new-raw) im-file
  "Creates a RAW image file. See also imErrorCodes."
  (file-name :string)
  (error :pointer))
