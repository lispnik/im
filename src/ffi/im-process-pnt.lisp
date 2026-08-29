;;;; src/ffi/im-process-pnt.lisp — DRAFTED by tools/gen-bindings.lisp.
;;;;
;;;; Source: im_process_pnt.h
;;;; Hand corrections below this line are expected and are kept;
;;;; re-run the generator into a clean tree and diff.

(in-package #:im.ffi)

;;; imToneGamutFlags
(cffi:defbitfield tone-gamut-flag
  (:tone-gamut-flag-minmax #x0100))

;;; imToneGamut
(cffi:defcenum tone-gamut
  :tone-gamut-normalize
  :tone-gamut-pow
  :tone-gamut-log
  :tone-gamut-exp
  :tone-gamut-invert
  :tone-gamut-zerostart
  :tone-gamut-solarize
  :tone-gamut-slice
  :tone-gamut-expand
  :tone-gamut-crop
  :tone-gamut-brightcont)

;;; imLogicOp
(cffi:defcenum logic-op
  :logic-op-and
  :logic-op-or
  :logic-op-xor
  :logic-op-nor)

;;; imBinaryOp
(cffi:defcenum binary-op
  :binary-op-add
  :binary-op-sub
  :binary-op-mul
  :binary-op-div
  :binary-op-diff
  :binary-op-pow
  :binary-op-min
  :binary-op-max)

;;; imUnaryOp
(cffi:defcenum unary-op
  :unary-op-eql
  :unary-op-abs
  :unary-op-less
  :unary-op-inv
  :unary-op-sqr
  :unary-op-sqrt
  :unary-op-log
  :unary-op-exp
  :unary-op-sin
  :unary-op-cos
  :unary-op-conj
  :unary-op-cpxnorm
  :unary-op-positives
  :unary-op-negatives)

(cffi:defcfun ("imProcessUnaryPointOp" %im-process-unary-point-op) :int
  "Apply an unary point operation using a custom function. One pixel from the
source affects the same pixel on target. Can be done in-place, images must
match size and depth. Data type can be different, but complex is not
supported. op_name is used only by the counter and can be NULL. Returns
zero if the counter aborted. In Lua, the params table is passed to the
function by using the Lua stack, so its table can contain any type of
objects, but they all must be unnamed."
  (src-image im-image)
  (dst-image im-image)
  (func :pointer)
  (params :pointer)
  (userdata :pointer)
  (op-name :string))

(cffi:defcfun ("imProcessUnaryPointColorOp" %im-process-unary-point-color-op) :int
  "Apply an unary point color operation using a custom function. One pixel
from the source affects the same pixel on target. Can be done in-place,
images must match size, depth can be different. Data type can be
different, but complex is not supported. op_name is used only by the
counter and can be NULL. Returns zero if the counter aborted. In Lua, the
params table is passed to the function by using the Lua stack, so its
table can contain any type of objects, but they all must be unnamed."
  (src-image im-image)
  (dst-image im-image)
  (func :pointer)
  (params :pointer)
  (userdata :pointer)
  (op-name :string))

(cffi:defcfun ("imProcessMultiPointOp" %im-process-multi-point-op) :int
  "Apply an multiple point operation using a custom function. One pixel from
each source affects the same pixel on target. All source images must match
in size, depth and data type. Can be done in-place, source and target must
match size and depth. Data type can be different between sources and
target, but complex is not supported. op_name is used only by the counter
and can be NULL. Returns zero if the counter aborted. In Lua, the params
table is passed to the function by using the Lua stack, so its table can
contain any type of objects, but they all must be unnamed."
  (src-image-list :pointer)
  (src-image-count :int)
  (dst-image im-image)
  (func :pointer)
  (params :pointer)
  (userdata :pointer)
  (op-name :string))

(cffi:defcfun ("imProcessMultiPointColorOp" %im-process-multi-point-color-op) :int
  "Apply an multiple point color operation using a custom function. One pixel
from each source affects the same pixel on target. All source images must
match in size, depth and data type. Can be done in-place, source and
target must match size, depth can be different. Data type can be different
between sources and target, but complex is not supported. op_name is used
only by the counter and can be NULL. Returns zero if the counter aborted.
In Lua, the params table is passed to the function by using the Lua stack,
so its table can contain any type of objects, but they all must be
unnamed."
  (src-image-list :pointer)
  (src-image-count :int)
  (dst-image im-image)
  (func :pointer)
  (params :pointer)
  (userdata :pointer)
  (op-name :string))

(cffi:defcfun ("imProcessUnArithmeticOp" %im-process-un-arithmetic-op) :void
  "Apply an arithmetic unary operation. Can be done in-place, images must
match color space and size. Target image can be several types depending on
source: \\li any integer -> any integer or real \\li real -> real \\li
complex -> complex If source is complex, target complex must be the same
data type (imcfloat-imcfloat or imcdouble-imcdouble only). If target is
byte, then the result is cropped to 0-255."
  (src-image im-image)
  (dst-image im-image)
  (op :int))

(cffi:defcfun ("imProcessArithmeticOp" %im-process-arithmetic-op) :void
  "Apply a binary arithmetic operation. Can be done in-place, images must
match color space and size. Source images must match, target image can be
several types depending on source: \\li any integer -> any integer+ or
real \\li real -> real \\li complex -> complex One exception is that you
can use src1=complex src2=real resulting dst=complex. If source is
complex, target complex must be the same data type (imcfloat-imcfloat or
imcdouble-imcdouble only). If target is integer then it must have equal or
more precision than the source. If target is byte, then the result is
cropped to 0-255. Alpha channel is not included. The New function will
create a new image of the same type of the source images."
  (src-image1 im-image)
  (src-image2 im-image)
  (dst-image im-image)
  (op :int))

(cffi:defcfun ("imProcessArithmeticConstOp" %im-process-arithmetic-const-op) :void
  "Apply a binary arithmetic operation with a constant value. Can be done
in-place, images must match color space and size. Target image can be
several types depending on source: \\li any integer -> any integer or real
\\li real -> real \\li complex -> complex The constant value is type
casted to an appropriate type before the operation. If source is complex,
target complex must be the same data type (imcfloat-imcfloat or
imcdouble-imcdouble only). If target is byte, then the result is cropped
to 0-255."
  (src-image im-image)
  (src-const :double)
  (dst-image im-image)
  (op :int))

(cffi:defcfun ("imProcessBlendConst" %im-process-blend-const) :void
  "Blend two images using an alpha value = [a * alpha + b * (1 - alpha)]. Can
be done in-place, images must match. alpha value must be in the interval
[0.0 - 1.0]."
  (src-image1 im-image)
  (src-image2 im-image)
  (dst-image im-image)
  (alpha :double))

(cffi:defcfun ("imProcessBlend" %im-process-blend) :void
  "Blend two images using an alpha channel = [a * alpha + b * (1 - alpha)].
Can be done in-place, images must match. alpha_image must have the same
data type except for complex images that must be real, and color_space
must be IM_GRAY. Maximum alpha values are based in imColorMax. Minimum is
always 0."
  (src-image1 im-image)
  (src-image2 im-image)
  (alpha-image im-image)
  (dst-image im-image))

(cffi:defcfun ("imProcessCompose" %im-process-compose) :void
  "Compose two images that have an alpha channel using the OVER operator. Can
be done in-place, images must match. Maximum alpha values are baed in
imColorMax. Minimum is always 0."
  (src-image1 im-image)
  (src-image2 im-image)
  (dst-image im-image))

(cffi:defcfun ("imProcessSplitComplex" %im-process-split-complex) :void
  "Split a complex image into two images with real and imaginary parts or
magnitude and phase parts (polar). Source image must be complex, target
images must be real."
  (src-image im-image)
  (dst-image1 im-image)
  (dst-image2 im-image)
  (polar :int))

(cffi:defcfun ("imProcessMergeComplex" %im-process-merge-complex) :void
  "Merges two images as the real and imaginary parts of a complex image, or
as magnitude and phase parts (polar = 1). Source images must be real,
target image must be complex."
  (src-image1 im-image)
  (src-image2 im-image)
  (dst-image im-image)
  (polar :int))

(cffi:defcfun ("imProcessMultipleMean" %im-process-multiple-mean) :void
  "Calculates the mean of multiple images. Images must match size and type."
  (src-image-list :pointer)
  (src-image-count :int)
  (dst-image im-image))

(cffi:defcfun ("imProcessMultipleStdDev" %im-process-multiple-std-dev) :void
  "Calculates the standard deviation of multiple images. Images must match
size and type. Use imProcessMultipleMean to calculate the mean_image."
  (src-image-list :pointer)
  (src-image-count :int)
  (mean-image im-image)
  (dst-image im-image))

(cffi:defcfun ("imProcessMultipleMedian" %im-process-multiple-median) :int
  "Calculates the median of multiple images. Images must match size and type.
Complex is not supported. Uses imProcessMultiPointOp internally."
  (src-image-list :pointer)
  (src-image-count :int)
  (dst-image im-image))

(cffi:defcfun ("imProcessAutoCovariance" %im-process-auto-covariance) :int
  "Calculates the auto-covariance of an image with the mean of a set of
images. Images must match. Returns zero if the counter aborted. Target is
IM_FLOAT, except if source is IM_DOUBLE. Returns zero if the counter
aborted."
  (src-image im-image)
  (mean-image im-image)
  (dst-image im-image))

(cffi:defcfun ("imProcessMultiplyConj" %im-process-multiply-conj) :void
  "Multiplies the conjugate of one complex image with another complex image.
Images must match size. Conj(img1) * img2 Can be done in-place."
  (src-image1 im-image)
  (src-image2 im-image)
  (dst-image im-image))

(cffi:defcfun ("imProcessBackSub" %im-process-back-sub) :void
  "Subtracts a background image using a tolerance. If different is less than
the tolerance background is detected and assigned to 0.\\ Else keeps the
original image or show the difference."
  (src-image1 im-image)
  (src-image2 im-image)
  (dst-image im-image)
  (tol :double)
  (show-diff :int))

(cffi:defcfun ("imProcessQuantizeRGBUniform" %im-process-quantize-rgb-uniform) :void
  "Converts a RGB image to a MAP image using uniform quantization. with an
optional 8x8 ordered dither. The RGB image must have data type IM_BYTE."
  (src-image im-image)
  (dst-image im-image)
  (do-dither :int))

(cffi:defcfun ("imProcessQuantizeRGBMedianCut" %im-process-quantize-rgb-median-cut) :void
  "Converts a RGB image to a MAP image using median cut quantization. The RGB
image must have data type IM_BYTE."
  (image im-image)
  (new-image im-image))

(cffi:defcfun ("imProcessQuantizeGrayUniform" %im-process-quantize-gray-uniform) :void
  "Quantizes a gray scale image in less that 256 grays using uniform
quantization. Both images should be IM_BYTE/IM_GRAY, the target can be
IM_MAP. Can be done in-place. The result is in the 0-255 range, except
when target is IM_MAP that is in the 0-(grays-1) range."
  (src-image im-image)
  (dst-image im-image)
  (grays :int))

(cffi:defcfun ("imProcessQuantizeGrayMedianCut" %im-process-quantize-gray-median-cut) :void
  "Quantizes a gray scale image in less that 256 grays using median cut
quantization. Both images should be IM_BYTE/IM_GRAY. Can be done in-place."
  (src-image im-image)
  (dst-image im-image)
  (grays :int))

(cffi:defcfun ("imProcessExpandHistogram" %im-process-expand-histogram) :void
  "Performs an histogram expansion based on a percentage of the number of
pixels. Percentage is used to obtain the amount of pixels of the lowest
level and the highest level, relative to the total of pixels. The
histogram is used an each level is summed while the result is less than
the obtained amount from 0 (for the lowest level) and from the last level
(for the highest). If it is zero, then only empty counts of the histogram
will be considered. Images must be (IM_BYTE, IM_SHORT or
IM_USHORT)/(IM_RGB or IM_GRAY). Can be done in-place. To expand the gamut
without using the histogram, by just specifying the lowest and highest
levels use the IM_GAMUT_EXPAND tone gamut operation (imProcessToneGamut)."
  (src-image im-image)
  (dst-image im-image)
  (percent :double))

(cffi:defcfun ("imProcessEqualizeHistogram" %im-process-equalize-histogram) :void
  "Performs an histogram equalization. Images must be (IM_BYTE, IM_SHORT or
IM_USHORT)/(IM_RGB or IM_GRAY). Can be done in-place."
  (src-image im-image)
  (dst-image im-image))

(cffi:defcfun ("imProcessSplitYChroma" %im-process-split-y-chroma) :void
  "Split a RGB image into luma and chroma. Chroma is calculated as
R-Y,G-Y,B-Y. Source image must be IM_RGB/IM_BYTE. luma image is
IM_GRAY/IM_BYTE and chroma is IM_RGB/IM_BYTE. Source and target must have
the same size."
  (src-image im-image)
  (y-image im-image)
  (chroma-image im-image))

(cffi:defcfun ("imProcessSplitHSI" %im-process-split-hsi) :void
  "Split a RGB image into HSI planes. Source image can be IM_RGB+IM_BYTE or
IM_RGB+IM_FLOAT/IM_DOUBLE only. Target images are all
IM_GRAY+IM_FLOAT/IM_DOUBLE. Source images must normalized to 0-1 if type
is IM_FLOAT/IM_DOUBLE (imProcessToneGamut can be used). See hsi for a
definition of the color conversion. Source and target must have the same
size."
  (src-image im-image)
  (h-image im-image)
  (s-image im-image)
  (i-image im-image))

(cffi:defcfun ("imProcessMergeHSI" %im-process-merge-hsi) :void
  "Merge HSI planes into a RGB image. Source images must be
IM_GRAY+IM_FLOAT/IM_DOUBLE. Target image can be IM_RGB+IM_BYTE or
IM_RGB+IM_FLOAT/IM_DOUBLE only. Source and target must have the same size.
See hsi for a definition of the color conversion."
  (h-image im-image)
  (s-image im-image)
  (i-image im-image)
  (dst-image im-image))

(cffi:defcfun ("imProcessSplitComponents" %im-process-split-components) :void
  "Split a multicomponent image into separate components, including alpha.
Target images must be IM_GRAY. Size and data types must be all the same.
The number of target images must match the depth of the source image,
including alpha."
  (src-image im-image)
  (dst-image-list :pointer))

(cffi:defcfun ("imProcessMergeComponents" %im-process-merge-components) :void
  "Merges separate components into a multicomponent image, including alpha.
Source images must be IM_GRAY. Size and data types must be all the same.
The number of source images must match the depth of the target image,
including alpha."
  (src-image-list :pointer)
  (dst-image im-image))

(cffi:defcfun ("imProcessNormalizeComponents" %im-process-normalize-components) :void
  "Normalize the color components by their sum. Example: c1 = c1/(c1+c2+c3).
It will not change the alpha channel if any. Target must be IM_FLOAT or
IM_DOUBLE."
  (src-image im-image)
  (dst-image im-image))

(cffi:defcfun ("imProcessReplaceColor" %im-process-replace-color) :void
  "Replaces the source color by the target color. The color will be type
casted to the image data type. The colors must have the same number of
components of the images. Supports all color spaces and all data types
except complex."
  (src-image im-image)
  (dst-image im-image)
  (src-color :pointer)
  (dst-color :pointer))

(cffi:defcfun ("imProcessSetAlphaColor" %im-process-set-alpha-color) :void
  "Sets the alpha channel in target where the given color occurs in source,
elsewhere alpha remains untouched. The color must have the same number of
components of the source image. If target does not have an alpha channel,
then its plane=0 is used. Supports all color spaces for source and all
data types except complex. Images must have the same size."
  (src-image im-image)
  (dst-image im-image)
  (src-color :pointer)
  (dst-alpha :double))

(cffi:defcfun ("imProcessPseudoColor" %im-process-pseudo-color) :void
  "Creates a pseudo color version of a GRAY image. Images must have same
size. Destiny must be IM_RGB/IM_BYTE. The colors are created from gray
values using them to index Hue angles from 0 to 360, and as Intensity
values, with maximum Saturation."
  (src-image im-image)
  (dst-image im-image))

(cffi:defcfun ("imProcessFixBGR" %im-process-fix-bgr) :void
  "Fix BGR order to RGB. Images must match. And must have color space RGB."
  (src-image im-image)
  (dst-image im-image))

(cffi:defcfun ("imProcessSelectHue" %im-process-select-hue) :void
  "Uses a hue interval to isolate where color predominates. Images must
match. And must have color space RGB."
  (src-image im-image)
  (dst-image im-image)
  (hue-start :double)
  (hue-end :double))

(cffi:defcfun ("imProcessSelectHSI" %im-process-select-hsi) :void
  "Uses a hue, saturation, intensity intervals to isolate where color
predominates. Images must match. And must have color space RGB."
  (src-image im-image)
  (dst-image im-image)
  (hue-start :double)
  (hue-end :double)
  (sat-start :double)
  (sat-end :double)
  (int-start :double)
  (int-end :double))

(cffi:defcfun ("imProcessBitwiseOp" %im-process-bitwise-op) :void
  "Apply a logical operation. Images must have data type integer. Can be done
in-place."
  (src-image1 im-image)
  (src-image2 im-image)
  (dst-image im-image)
  (op :int))

(cffi:defcfun ("imProcessBitwiseNot" %im-process-bitwise-not) :void
  "Apply a logical NOT operation. Images must have data type integer. Can be
done in-place."
  (src-image im-image)
  (dst-image im-image))

(cffi:defcfun ("imProcessBitMask" %im-process-bit-mask) :void
  "Apply a bit mask. The same as imProcessBitwiseOp but the second image is
replaced by a fixed mask. Images must have data type IM_BYTE. It is valid
only for AND, OR and XOR. Can be done in-place. In Lua, mask is a string
with 0s and 1s, for example: \"11001111\"."
  (src-image im-image)
  (dst-image im-image)
  (mask :unsigned-char)
  (op :int))

(cffi:defcfun ("imProcessBitPlane" %im-process-bit-plane) :void
  "Extract or Reset a bit plane. For ex: 000X0000 or XXX0XXXX (plane=3).
Images must have data type IM_BYTE. Can be done in-place."
  (src-image im-image)
  (dst-image im-image)
  (plane :int)
  (do-reset :int))

(cffi:defcfun ("imProcessRenderOp" %im-process-render-op) :int
  "Render a synthetic image using a render function. plus will make the
render be added to the current image data, or else all data will be
replaced. All the render functions use this or the conditional function.
Returns zero if the counter aborted."
  (image im-image)
  (func :pointer)
  (render-name :string)
  (params :pointer)
  (plus :int))

(cffi:defcfun ("imProcessRenderOpAlpha" %im-process-render-op-alpha) :int
  "Same as imProcessRenderOp but with alpha channel support. (since 3.14) Can
also be used if the image does not have alpha."
  (image im-image)
  (func :pointer)
  (render-name :string)
  (params :pointer)
  (plus :int))

(cffi:defcfun ("imProcessRenderCondOp" %im-process-render-cond-op) :int
  "Render a synthetic image using a conditional render function. Data will be
rendered only if the conditional parameter is true. Returns zero if the
counter aborted."
  (image im-image)
  (func :pointer)
  (render-name :string)
  (params :pointer))

(cffi:defcfun ("imProcessRenderCondOpAlpha" %im-process-render-cond-op-alpha) :int
  "Same as imProcessRenderOp but with alpha channel support. (since 3.14) Can
also be used if the image does not have alpha."
  (image im-image)
  (func :pointer)
  (render-name :string)
  (params :pointer))

(cffi:defcfun ("imProcessRenderAddSpeckleNoise" %im-process-render-add-speckle-noise) :int
  "Render speckle noise on existing data. Can be done in-place."
  (src-image im-image)
  (dst-image im-image)
  (percent :double))

(cffi:defcfun ("imProcessRenderAddGaussianNoise" %im-process-render-add-gaussian-noise) :int
  "Render gaussian noise on existing data. Can be done in-place."
  (src-image im-image)
  (dst-image im-image)
  (mean :double)
  (stddev :double))

(cffi:defcfun ("imProcessRenderAddUniformNoise" %im-process-render-add-uniform-noise) :int
  "Render uniform noise on existing data. Can be done in-place."
  (src-image im-image)
  (dst-image im-image)
  (mean :double)
  (stddev :double))

(cffi:defcfun ("imProcessRenderRandomNoise" %im-process-render-random-noise) :int
  "Render random noise."
  (image im-image))

(cffi:defcfun ("imProcessRenderConstant" %im-process-render-constant) :int
  "Render a constant. The number of values must match the depth of the image.
Value must have the same number of the image depth including alpha. Alpha
channel is supported (since 3.14)."
  (image im-image)
  (value :pointer))

(cffi:defcfun ("imProcessRenderWheel" %im-process-render-wheel) :int
  "Render a centered wheel."
  (image im-image)
  (internal-radius :int)
  (external-radius :int))

(cffi:defcfun ("imProcessRenderCone" %im-process-render-cone) :int
  "Render a centered cone."
  (image im-image)
  (radius :int))

(cffi:defcfun ("imProcessRenderTent" %im-process-render-tent) :int
  "Render a centered tent."
  (image im-image)
  (tent-width :int)
  (tent-height :int))

(cffi:defcfun ("imProcessRenderRamp" %im-process-render-ramp) :int
  "Render a ramp. Direction can be vertical (1) or horizontal (0)."
  (image im-image)
  (start :int)
  (end :int)
  (vert-dir :int))

(cffi:defcfun ("imProcessRenderBox" %im-process-render-box) :int
  "Render a centered box."
  (image im-image)
  (box-width :int)
  (box-height :int))

(cffi:defcfun ("imProcessRenderSinc" %im-process-render-sinc) :int
  "Render a centered sinc."
  (image im-image)
  (x-period :double)
  (y-period :double))

(cffi:defcfun ("imProcessRenderGaussian" %im-process-render-gaussian) :int
  "Render a centered gaussian."
  (image im-image)
  (stddev :double))

(cffi:defcfun ("imProcessRenderLapOfGaussian" %im-process-render-lap-of-gaussian) :int
  "Render the laplacian of a centered gaussian."
  (image im-image)
  (stddev :double))

(cffi:defcfun ("imProcessRenderCosine" %im-process-render-cosine) :int
  "Render a centered cosine."
  (image im-image)
  (x-period :double)
  (y-period :double))

(cffi:defcfun ("imProcessRenderGrid" %im-process-render-grid) :int
  "Render a centered grid."
  (image im-image)
  (x-space :int)
  (y-space :int))

(cffi:defcfun ("imProcessRenderChessboard" %im-process-render-chessboard) :int
  "Render a centered chessboard."
  (image im-image)
  (x-space :int)
  (y-space :int))

(cffi:defcfun ("imProcessRenderFloodFill" %im-process-render-flood-fill) :void
  "Render a color or gray flood fill. If image has the IM_RGB color space,
then replace_color must have 3 components, or 4 when alpha is present. If
image has the IM_GRAY or IM_MAP color space, then replace_color must have
1 component. For IM_MAP images the colors in the palette will be compared
instead of the indices (since 3.14). Alpha channel is supported in IM_RGB
images (since 3.14), alpha will also be considered in the comparison."
  (image im-image)
  (start-x :int)
  (start-y :int)
  (replace-color :pointer)
  (tolerance :double))

(cffi:defcfun ("imProcessToneGamut" %im-process-tone-gamut) :void
  "Apply a gamut operation with arguments. Supports all data types except
complex. For IM_GAMUT_NORMALIZE when min > 0 and max < 1, it will just do
a copy. IM_BYTE images have min=0 and max=255 always. To control min and
max values use the IM_GAMUT_MINMAX flag. Can be done in-place. When there
is no extra parameters, params can use NULL. Alpha is not changed if
present. See also imageenhance."
  (src-image im-image)
  (dst-image im-image)
  (op :int)
  (params :pointer))

(cffi:defcfun ("imProcessUnNormalize" %im-process-un-normalize) :void
  "Converts from (0-1) to (0-255), crop out of bounds values. Source image
must be real, and target image must be IM_BYTE."
  (src-image im-image)
  (dst-image im-image))

(cffi:defcfun ("imProcessDirectConv" %im-process-direct-conv) :void
  "Directly converts integer and real data types into IM_BYTE images. This
can also be done using imConvertDataType with IM_CAST_DIRECT flag."
  (src-image im-image)
  (dst-image im-image))

(cffi:defcfun ("imProcessNegative" %im-process-negative) :void
  "A negative effect. Uses imProcessToneGamut with IM_GAMUT_INVERT for non
MAP images. Supports all color spaces and all data types except complex.
Can be done in-place."
  (src-image im-image)
  (dst-image im-image))

(cffi:defcfun ("imProcessCalcAutoGamma" %im-process-calc-auto-gamma) :double
  "Calculates an automatic gamma factor.
gamma=log((mean-min)/(max-min))/log(0.5); Usefull for imProcessToneGamut
when using IM_GAMUT_POW."
  (image im-image))

(cffi:defcfun ("imProcessShiftHSI" %im-process-shift-hsi) :void
  "Apply a shift using HSI coordinates. Supports all data types except
complex. shift is between -1.0 and 1.0, except for Hue where shift is in
degrees. Can be done in-place."
  (src-image im-image)
  (dst-image im-image)
  (h-shift :double)
  (s-shift :double)
  (i-shift :double))

(cffi:defcfun ("imProcessShiftComponent" %im-process-shift-component) :void
  "Apply a shift to the components in normalized space 0-1. Supports all data
types except complex. shift is between -1.0 and 1.0 Can be done in-place."
  (src-image im-image)
  (dst-image im-image)
  (c0-shift :double)
  (c1-shift :double)
  (c2-shift :double))

(cffi:defcfun ("imProcessThreshold" %im-process-threshold) :void
  "Apply a manual threshold. threshold = a <= level ? 0: value Normal value
is 1 but another common value is 255. Can be done in-place for IM_BYTE
source. Source color space must be IM_GRAY, and target color space must be
IM_BINARY. complex is not supported."
  (src-image im-image)
  (dst-image im-image)
  (level :double)
  (value :int))

(cffi:defcfun ("imProcessThresholdByDiff" %im-process-threshold-by-diff) :void
  "Apply a threshold by the difference of two images. threshold = a1 <= a2 ?
0: 1 Source color space must be IM_GRAY, and target color space must be
IM_BINARY. complex is not supported. Can be done in-place for IM_BYTE
source."
  (src-image1 im-image)
  (src-image2 im-image)
  (dst-image im-image))

(cffi:defcfun ("imProcessHysteresisThreshold" %im-process-hysteresis-threshold) :void
  "Apply a threshold by the Hysteresis method. Hysteresis thersholding of
edge pixels. Starting at pixels with a value greater than the HIGH
threshold, trace a connected sequence of pixels that have a value greater
than the LOW threhsold. complex is not supported. Can be done in-place for
IM_BYTE source. Note: could not find the original source code author name."
  (src-image im-image)
  (dst-image im-image)
  (low-thres :int)
  (high-thres :int))

(cffi:defcfun ("imProcessHysteresisThresEstimate" %im-process-hysteresis-thres-estimate) :void
  "Estimates hysteresis low and high threshold levels. Image data type can be
IM_BYTE, IM_SHORT or IM_USHORT. Usefull for imProcessHysteresisThreshold."
  (image im-image)
  (low-level :pointer)
  (high-level :pointer))

(cffi:defcfun ("imProcessUniformErrThreshold" %im-process-uniform-err-threshold) :int
  "Calculates the threshold level for manual threshold using an uniform error
approach. Supports only IM_BYTE images. Extracted from XITE, Copyright
1991, Blab, UiO http://www.ifi.uio.no/~blab/Software/Xite/ Returns the
used level."
  (src-image im-image)
  (dst-image im-image))

(cffi:defcfun ("imProcessDiffusionErrThreshold" %im-process-diffusion-err-threshold) :void
  "Apply a dithering on each image channel by using a diffusion error method.
It can be applied on any IM_BYTE images. It will \"threshold\" each
channel indivudually, so source and target must be of the same depth. Not
using OpenMP when enabled."
  (src-image im-image)
  (dst-image im-image)
  (level :int))

(cffi:defcfun ("imProcessPercentThreshold" %im-process-percent-threshold) :int
  "Calculates the threshold level for manual threshold using a percentage of
pixels that should stay bellow the threshold. Image data type can be
IM_BYTE, IM_SHORT or IM_USHORT. Source color space must be IM_GRAY, and
target color space must be IM_BINARY. Returns the used level."
  (src-image im-image)
  (dst-image im-image)
  (percent :double))

(cffi:defcfun ("imProcessOtsuThreshold" %im-process-otsu-threshold) :int
  "Calculates the threshold level for manual threshold using the Otsu
approach. Image can be IM_BYTE, IM_SHORT or IM_USHORT. Source color space
must be IM_GRAY, and target color space must be IM_BINARY. Returns the
used level. Original implementation by Flavio Szenberg."
  (src-image im-image)
  (dst-image im-image))

(cffi:defcfun ("imProcessMinMaxThreshold" %im-process-min-max-threshold) :double
  "Calculates the threshold level for manual threshold using (max-min)/2.
Returns the used level. Source color space must be IM_GRAY, and target
color space must be IM_BINARY. complex is not supported. Can be done
in-place for IM_BYTE source."
  (src-image im-image)
  (dst-image im-image))

(cffi:defcfun ("imProcessLocalMaxThresEstimate" %im-process-local-max-thres-estimate) :void
  "Estimates Local Max threshold level for images. Image can be IM_BYTE,
IM_SHORT or IM_USHORT."
  (image im-image)
  (level :pointer))

(cffi:defcfun ("imProcessSliceThreshold" %im-process-slice-threshold) :void
  "Apply a manual threshold using an interval. threshold = start_level <= a
<= end_level ? 1: 0 Normal value is 1 but another common value is 255.
Source color space must be IM_GRAY, and target color space must be
IM_BINARY. complex is not supported. Can be done in-place for IM_BYTE
source."
  (src-image im-image)
  (dst-image im-image)
  (start-level :double)
  (end-level :double))

(cffi:defcfun ("imProcessThresholdColor" %im-process-threshold-color) :void
  "Threshold using a color and a tolerance value. The color will be type
casted to the image data type. The color must have the same number of
components of the images. Supports all color spaces and all data types
except complex."
  (src-image im-image)
  (dst-image im-image)
  (src-color :pointer)
  (tol :double))

(cffi:defcfun ("imProcessThresholdSaturation" %im-process-threshold-saturation) :void
  "Threshold using a saturation minimum. (since 3.14) Supports only
IM_RGB+IM_BYTE as source."
  (src-image im-image)
  (dst-image im-image)
  (s-min :double))

(cffi:defcfun ("imProcessPixelate" %im-process-pixelate) :void
  "Generates a zoom in effect averaging colors inside a square region.
Operates only on IM_BYTE images."
  (src-image im-image)
  (dst-image im-image)
  (box-size :int))

(cffi:defcfun ("imProcessPosterize" %im-process-posterize) :void
  "A simple Posterize effect. It reduces the number of colors in the image
eliminating less significant bit planes. Can have 1 to 7 levels. See
imProcessBitMask. Images must have data type IM_BYTE."
  (src-image im-image)
  (dst-image im-image)
  (level :int))

(cffi:defcfun ("imProcessBinaryMask" %im-process-binary-mask) :void
  "Applies a binary mask to an image. The mask must be a IM_BINARY image.
Where the mask is 1, the original image is preserved. Where it is 0, the
value is replaced by the minimum (0 for imbyte images). Can be done
in-place."
  (src-image im-image)
  (dst-image im-image)
  (mask-image im-image))

(cffi:defcfun ("imProcessNormDiffRatio" %im-process-norm-diff-ratio) :void
  "Calculates the Normalized Difference Ratio. Uses the formula NormDiffRatio
= (a-b)/(a+b), The result image has [-1,1] interval. Images must be
IM_GRAY, and the target image must be IM_FLOAT, except if source is
IM_DOUBLE."
  (image1 im-image)
  (image2 im-image)
  (dst-image im-image))

(cffi:defcfun ("imProcessAbnormalHyperionCorrection" %im-process-abnormal-hyperion-correction) :void
  "Applies the abnormal pixel correction as described in the article. (Since
3.8) Images must be IM_GRAY. Source and Target must have the same
datatype, and complex is not supported. image_abnormal is optional, can be
NULL. If not NULL, must be IM_BINARY and it will store the abnormal pixels
distribution. Can be done in-place. threshold_percent is the percentage of
the height that must have abnormal pixels candidates.
threshold_consecutive is the minimum number of consecutive abnormal pixels
candidates to be considered an abnormal range. (usually the longest
vertical ground feature in pixels) * Based on \"Detection and Correction
of Abnormal Pixels in Hyperion Images\" from T. Han, D. G. Goodenough, A.
Dyk, and J. Love"
  (src-image im-image)
  (dst-image im-image)
  (threshold-consecutive :int)
  (threshold-percent :int)
  (image-abnormal im-image))

(cffi:defcfun ("imProcessConvertDataType" %im-process-convert-data-type) :int
  "Same as imConvertDataType."
  (src-image im-image)
  (dst-image im-image)
  (cpx2real :int)
  (gamma :double)
  (absolute :int)
  (cast-mode :int))

(cffi:defcfun ("imProcessConvertColorSpace" %im-process-convert-color-space) :int
  "Same as imConvertColorSpace."
  (src-image im-image)
  (dst-image im-image))

(cffi:defcfun ("imProcessConvertToBitmap" %im-process-convert-to-bitmap) :int
  "Same as imConvertToBitmap."
  (src-image im-image)
  (dst-image im-image)
  (cpx2real :int)
  (gamma :double)
  (absolute :int)
  (cast-mode :int))
