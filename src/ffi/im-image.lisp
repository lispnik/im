;;;; src/ffi/im-image.lisp — DRAFTED by tools/gen-bindings.lisp.
;;;;
;;;; Source: im_image.h
;;;; Hand corrections below this line are expected and are kept;
;;;; re-run the generator into a clean tree and diff.

(in-package #:im.ffi)

(cffi:defcfun ("imImageCreate" %im-image-create) im-image
  "Creates a new image. See also imDataType and imColorSpace. Image data is
cleared as imImageClear. In Lua the IM image metatable name is
\"imImage\". When converted to a string will return \"imImage(%p)
[width=%d,height=%d,color_space=%s,data_type=%s,depth=%d]\" where %p is
replaced by the userdata address, and other values are replaced by the
respective attributes. If the image is already destroyed by
im.ImageDestroy, then it will return also the suffix \"-destroyed\"."
  (width :int)
  (height :int)
  (color-space :int)
  (data-type :int))

(cffi:defcfun ("imImageInit" %im-image-init) im-image
  "Initializes the image structure but does not allocates image data. See
also imDataType and imColorSpace. The only addtional flag thar color_mode
can has here is IM_ALPHA. To release the image structure without releasing
the buffer, set \"data[0]\" to NULL before calling imImageDestroy."
  (width :int)
  (height :int)
  (color-mode :int)
  (data-type :int)
  (data-buffer :pointer)
  (palette :pointer)
  (palette-count :int))

(cffi:defcfun ("imImageCreateBased" %im-image-create-based) im-image
  "Creates a new image based on an existing one. If the addicional parameters
are -1, the given image parameters are used. The image atributes always
are copied. HasAlpha is copied. See also imDataType and imColorSpace. The
addicional parameters in Lua can be nil, and they can also be functions
with the based image as a parameter to return the respective value."
  (image im-image)
  (width :int)
  (height :int)
  (color-space :int)
  (data-type :int))

(cffi:defcfun ("imImageDestroy" %im-image-destroy) :void
  "Destroys the image and frees the memory used. image data is destroyed only
if its data[0] is not NULL. In Lua if this function is not called, the
image is destroyed by the garbage collector."
  (image im-image))

(cffi:defcfun ("imImageAddAlpha" %im-image-add-alpha) :void
  "Adds an alpha channel plane and sets its value to 0 (transparent)."
  (image im-image))

(cffi:defcfun ("imImageSetAlpha" %im-image-set-alpha) :void
  "Sets the alpha channel plane to a constant."
  (image im-image)
  (alpha :double))

(cffi:defcfun ("imImageRemoveAlpha" %im-image-remove-alpha) :void
  "Removes the alpha channel plane if any."
  (image im-image))

(cffi:defcfun ("imImageReshape" %im-image-reshape) :void
  "Changes the buffer size. Reallocate internal buffers if the new size is
larger than the original."
  (image im-image)
  (width :int)
  (height :int))

(cffi:defcfun ("imImageCopy" %im-image-copy) :void
  "Copy image data and attributes from one image to another. Images must have
the same size and type."
  (src-image im-image)
  (dst-image im-image))

(cffi:defcfun ("imImageCopyData" %im-image-copy-data) :void
  "Copy image data only fom one image to another. Images must have the same
size and type."
  (src-image im-image)
  (dst-image im-image))

(cffi:defcfun ("imImageCopyAttributes" %im-image-copy-attributes) :void
  "Copies the image attributes from src to dst. Includes the pallete if
defined in both images."
  (src-image im-image)
  (dst-image im-image))

(cffi:defcfun ("imImageMergeAttributes" %im-image-merge-attributes) :void
  "Merges the image attributes from src to dst. Attributes that exist in dst
are not replaced. Doens NOT include the pallete."
  (src-image im-image)
  (dst-image im-image))

(cffi:defcfun ("imImageCopyPlane" %im-image-copy-plane) :void
  "Copy one image plane fom one image to another. Images must have the same
size and type."
  (src-image im-image)
  (src-plane :int)
  (dst-image im-image)
  (dst-plane :int))

(cffi:defcfun ("imImageDuplicate" %im-image-duplicate) im-image
  "Creates a copy of the image."
  (image im-image))

(cffi:defcfun ("imImageClone" %im-image-clone) im-image
  "Creates a clone of the image. i.e. same attributes but ignore contents."
  (image im-image))

(cffi:defcfun ("imImageSetAttribute" %im-image-set-attribute) :void
  "Changes an extended attribute. The data will be internally duplicated. If
data is NULL and count==0 the attribute is removed. If count is -1 and
data_type is IM_BYTE then data is zero terminated. See also imDataType. If
data_type is IM_BYTE, a string can be used as data."
  (image im-image)
  (attrib :string)
  (data-type :int)
  (count :int)
  (data :pointer))

(cffi:defcfun ("imImageSetAttribInteger" %im-image-set-attrib-integer) :void
  "Changes an extended attribute as an integer."
  (image im-image)
  (attrib :string)
  (data-type :int)
  (value :int))

(cffi:defcfun ("imImageSetAttribReal" %im-image-set-attrib-real) :void
  "Changes an extended attribute as a real."
  (image im-image)
  (attrib :string)
  (data-type :int)
  (value :double))

(cffi:defcfun ("imImageSetAttribString" %im-image-set-attrib-string) :void
  "Changes an extended attribute as a string."
  (image im-image)
  (attrib :string)
  (value :string))

(cffi:defcfun ("imImageGetAttribute" %im-image-get-attribute) :pointer
  "Returns an extended attribute. Returns NULL if not found. See also
imDataType. If data_type is IM_BYTE, as_string can be used to return a
string instead of a table."
  (image im-image)
  (attrib :string)
  (data-type :pointer)
  (count :pointer))

(cffi:defcfun ("imImageGetAttribInteger" %im-image-get-attrib-integer) :int
  "Returns an extended attribute as an integer."
  (image im-image)
  (attrib :string)
  (index :int))

(cffi:defcfun ("imImageGetAttribReal" %im-image-get-attrib-real) :double
  "Returns an extended attribute as a real."
  (image im-image)
  (attrib :string)
  (index :int))

(cffi:defcfun ("imImageGetAttribString" %im-image-get-attrib-string) :string
  "Returns an extended attribute as a string."
  (image im-image)
  (attrib :string))

(cffi:defcfun ("imImageGetAttributeList" %im-image-get-attribute-list) :void
  "Returns a list of the attribute names. \"attrib\" must contain room enough
for \"attrib_count\" names. Use \"attrib=NULL\" to return only the count.
The array receives pointers to the names held inside the attribute table,
not copies, so do not free them. They are also only valid while those
entries exist: setting or removing any attribute, or destroying the owner,
can free a name a previous call handed out. Copy anything you need to keep
across a change to the attributes."
  (image im-image)
  (attrib :pointer)
  (attrib-count :pointer))

(cffi:defcfun ("imImageClear" %im-image-clear) :void
  "Sets all image data to zero. But if color space is YCBCR, LAB or LUV, and
data type is BYTE or USHORT, then data is initialized with 128 or 32768
accordingly. Alpha is initialized as transparent (0)."
  (image im-image))

(cffi:defcfun ("imImageIsBitmap" %im-image-is-bitmap) :int
  "Indicates that the image can be viewed in common graphic devices. Data
type must be IM_BYTE. Color mode can be IM_RGB, IM_MAP, IM_GRAY or
IM_BINARY."
  (image im-image))

(cffi:defcfun ("imImageSetPalette" %im-image-set-palette) :void
  "Changes the image palette. This will destroy the existing palette and
replace it with the given palette pointer. Only the pointer is stored, so
the palette should be a new palette and it can not be a static array."
  (image im-image)
  (palette :pointer)
  (palette-count :int))

(cffi:defcfun ("imImageMatchSize" %im-image-match-size) :int
  "Returns 1 if the images match width and height. Returns 0 otherwise."
  (image1 im-image)
  (image2 im-image))

(cffi:defcfun ("imImageMatchColor" %im-image-match-color) :int
  "Returns 1 if the images match color mode and data type. Returns 0
otherwise."
  (image1 im-image)
  (image2 im-image))

(cffi:defcfun ("imImageMatchDataType" %im-image-match-data-type) :int
  "Returns 1 if the images match width, height and data type. Returns 0
otherwise."
  (image1 im-image)
  (image2 im-image))

(cffi:defcfun ("imImageMatchColorSpace" %im-image-match-color-space) :int
  "Returns 1 if the images match width, height and color space. Returns 0
otherwise."
  (image1 im-image)
  (image2 im-image))

(cffi:defcfun ("imImageMatch" %im-image-match) :int
  "Returns 1 if the images match in width, height, data type and color space.
Returns 0 otherwise."
  (image1 im-image)
  (image2 im-image))

(cffi:defcfun ("imImageSetMap" %im-image-set-map) :void
  "Changes the image color space to map by just changing color_space. Image
must be BINARY or GRAY/BYTE."
  (image im-image))

(cffi:defcfun ("imImageSetBinary" %im-image-set-binary) :void
  "Changes the image color space to binary by just changing color_space and
the palette. Image must be MAP or GRAY/BYTE."
  (image im-image))

(cffi:defcfun ("imImageSetGray" %im-image-set-gray) :void
  "Changes the image color space to gray by just changing color_space and the
palette. Image must be BINARY or MAP. Palette is changed only if image was
BINARY."
  (image im-image))

(cffi:defcfun ("imImageMakeBinary" %im-image-make-binary) :void
  "Changes a gray BYTE data (0,255) into a binary data (0,1), done in-place.
Color space is not changed. Data type must be IM_BYTE."
  (image im-image))

(cffi:defcfun ("imImageMakeGray" %im-image-make-gray) :void
  "Changes a binary data (0,1) into a gray BYTE data (0,255), done in-place.
Color space is not changed. Data type must be IM_BYTE."
  (image im-image))

(cffi:defcfun ("imFileLoadImage" %im-file-load-image) im-image
  "Loads an image from an already open file. Returns NULL if failed. This
will call imFileReadImageInfo and imFileReadImageData. index specifies the
image number between 0 and image_count-1. The returned image will be of
the same color_space and data_type of the image in the file. Attributes
from the file will be stored at the image. See also imErrorCodes. Default
index is 0."
  (ifile im-file)
  (index :int)
  (error :pointer))

(cffi:defcfun ("imFileLoadImageFrame" %im-file-load-image-frame) :void
  "Loads an image from an already open file. Returns NULL if failed. This
function assumes that the image in the file has the same parameters as the
given image. This will call imFileReadImageInfo and imFileReadImageData.
index specifies the image number between 0 and image_count-1. The returned
image will be of the same color_space and data_type of the image in the
file. Attributes from the file will be stored at the image. See also
imErrorCodes. Default index is 0."
  (ifile im-file)
  (index :int)
  (image im-image)
  (error :pointer))

(cffi:defcfun ("imFileLoadBitmap" %im-file-load-bitmap) im-image
  "Loads an image from an already open file, but forces the image to be a
bitmap. The returned imagem will be always a Bitmap image, with
color_space RGB, MAP, GRAY or BINARY, and data_type IM_BYTE. index
specifies the image number between 0 and image_count-1. Returns NULL if
failed. Attributes from the file will be stored at the image. See also
imErrorCodes. Default index is 0."
  (ifile im-file)
  (index :int)
  (error :pointer))

(cffi:defcfun ("imFileLoadImageRegion" %im-file-load-image-region) im-image
  "Loads an image region from an already open file. Returns NULL if failed.
This will call imFileReadImageInfo and imFileReadImageData. index
specifies the image number between 0 and image_count-1. The returned image
will be of the same color_space and data_type of the image in the file, or
will be a Bitmap image. Attributes from the file will be stored at the
image. See also imErrorCodes. For now, it works only for the ECW file
format. Default index is 0."
  (ifile im-file)
  (index :int)
  (bitmap :int)
  (error :pointer)
  (xmin :int)
  (xmax :int)
  (ymin :int)
  (ymax :int)
  (width :int)
  (height :int))

(cffi:defcfun ("imFileLoadBitmapFrame" %im-file-load-bitmap-frame) :void
  "Loads an image from an already open file, but forces the image to be a
bitmap. This function assumes that the image in the file has the same
parameters as the given image. The imagem must be a Bitmap image, with
color_space RGB, MAP, GRAY or BINARY, and data_type IM_BYTE. index
specifies the image number between 0 and image_count-1. Returns NULL if
failed. Attributes from the file will be stored at the image. See also
imErrorCodes. Default index is 0."
  (ifile im-file)
  (index :int)
  (image im-image)
  (error :pointer))

(cffi:defcfun ("imFileSaveImage" %im-file-save-image) :int
  "Saves the image to an already open file. This will call
imFileWriteImageInfo and imFileWriteImageData. Attributes from the image
will be stored at the file. Returns error code."
  (ifile im-file)
  (image im-image))

(cffi:defcfun ("imFileImageLoad" %im-file-image-load) im-image
  "Loads an image from file. Open, loads and closes the file. index specifies
the image number between 0 and image_count-1. Returns NULL if failed.
Attributes from the file will be stored at the image. See also
imErrorCodes. Default index is 0."
  (file-name :string)
  (index :int)
  (error :pointer))

(cffi:defcfun ("imFileImageLoadBitmap" %im-file-image-load-bitmap) im-image
  "Loads an image from file, but forces the image to be a bitmap. Open, loads
and closes the file. index specifies the image number between 0 and
image_count-1. Returns NULL if failed. Attributes from the file will be
stored at the image. See also imErrorCodes. Default index is 0."
  (file-name :string)
  (index :int)
  (error :pointer))

(cffi:defcfun ("imFileImageLoadRegion" %im-file-image-load-region) im-image
  "Loads an image region from file. Open, loads and closes the file. index
specifies the image number between 0 and image_count-1. Returns NULL if
failed. Attributes from the file will be stored at the image. See also
imErrorCodes. For now, it works only for the ECW file format. Default
index is 0."
  (file-name :string)
  (index :int)
  (bitmap :int)
  (error :pointer)
  (xmin :int)
  (xmax :int)
  (ymin :int)
  (ymax :int)
  (width :int)
  (height :int))

(cffi:defcfun ("imFileImageSave" %im-file-image-save) :int
  "Saves the image to file. Open, saves and closes the file. Returns error
code. Attributes from the image will be stored at the file."
  (file-name :string)
  (format :string)
  (image im-image))
