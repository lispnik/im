;;;; src/ffi/im-process-loc.lisp — DRAFTED by tools/gen-bindings.lisp.
;;;;
;;;; Source: im_process_loc.h
;;;; Hand corrections below this line are expected and are kept;
;;;; re-run the generator into a clean tree and diff.

(in-package #:im.ffi)

(cffi:defcfun ("imProcessReduce" %im-process-reduce) :int
  "Only reduze the image size using the given decimation order. Supported
decimation orders: \\li 0 - zero order (mean) [default in Lua for MAP and
BINARY] \\li 1 - first order (bilinear decimation) [default in Lua] Images
must be of the same type. If image type is IM_MAP or IM_BINARY, must use
order=0. Returns zero if the counter aborted."
  (src-image im-image)
  (dst-image im-image)
  (order :int))

(cffi:defcfun ("imProcessResize" %im-process-resize) :int
  "Change the image size using the given interpolation order. Supported
interpolation orders: \\li 0 - zero order (near neighborhood) [default in
Lua for MAP and BINARY] \\li 1 - first order (bilinear interpolation)
[default in Lua] \\li 3 - third order (bicubic interpolation) Images must
be of the same type. If image type is IM_MAP or IM_BINARY, must use
order=0. Returns zero if the counter aborted."
  (src-image im-image)
  (dst-image im-image)
  (order :int))

(cffi:defcfun ("imProcessReduceBy4" %im-process-reduce-by4) :int
  "Reduze the image area by 4 (w/2,h/2). Uses a fast average of neighbors.
Images must be of the same type. Target image size must be source image
width/2, height/2. Can not operate on IM_MAP nor IM_BINARY images. Returns
zero if the counter aborted."
  (src-image im-image)
  (dst-image im-image))

(cffi:defcfun ("imProcessCrop" %im-process-crop) :int
  "Extract a rectangular region from an image. Images must be of the same
type. Target image size must be smaller than source image width-xmin,
height-ymin. ymin and xmin must be >0 and <size. Returns zero if the
counter aborted."
  (src-image im-image)
  (dst-image im-image)
  (xmin :int)
  (ymin :int))

(cffi:defcfun ("imProcessInsert" %im-process-insert) :int
  "Insert a rectangular region in an image. Images must be of the same type.
Region image size can be larger than source image. ymin and xmin must be
>0 and <size. Source and target must be of the same size. Can be done
in-place. Returns zero if the counter aborted."
  (src-image im-image)
  (region-image im-image)
  (dst-image im-image)
  (xmin :int)
  (ymin :int))

(cffi:defcfun ("imProcessAddMargins" %im-process-add-margins) :int
  "Increase the image size by adding pixels with zero value. Images must be
of the same type. Target image size must be greatter or equal than source
image width+xmin, height+ymin. Returns zero if the counter aborted."
  (src-image im-image)
  (dst-image im-image)
  (xmin :int)
  (ymin :int))

(cffi:defcfun ("imProcessCalcRotateSize" %im-process-calc-rotate-size) :void
  "Calculates the size of the new image after rotation."
  (width :int)
  (height :int)
  (new-width :pointer)
  (new-height :pointer)
  (cos0 :double)
  (sin0 :double))

(cffi:defcfun ("imProcessRotate" %im-process-rotate) :int
  "Rotates the image using the given interpolation order (see
imProcessResize). Images must be of the same type. The target size can be
calculated using imProcessCalcRotateSize to fit the new image size, or can
be any size, including the original size. The rotation is relative to the
center of the image. Returns zero if the counter aborted."
  (src-image im-image)
  (dst-image im-image)
  (cos0 :double)
  (sin0 :double)
  (order :int))

(cffi:defcfun ("imProcessRotateRef" %im-process-rotate-ref) :int
  "Rotates the image using the given interpolation order (see
imProcessResize). Images must be of the same type. Target can have any
size, including the original size. The rotation is relative to the
reference point. But the result can be shifted to the origin. Returns zero
if the counter aborted."
  (src-image im-image)
  (dst-image im-image)
  (cos0 :double)
  (sin0 :double)
  (x :int)
  (y :int)
  (to-origin :int)
  (order :int))

(cffi:defcfun ("imProcessRotate90" %im-process-rotate90) :int
  "Rotates the image in 90 degrees counterclockwise or clockwise. Swap
columns by lines. Images must be of the same type. Target width and height
must be source height and width. Direction can be clockwise (1) or counter
clockwise (-1). Returns zero if the counter aborted."
  (src-image im-image)
  (dst-image im-image)
  (dir :int))

(cffi:defcfun ("imProcessRotate180" %im-process-rotate180) :int
  "Rotates the image in 180 degrees. Swap columns and swap lines. Images must
be of the same type and size. Returns zero if the counter aborted."
  (src-image im-image)
  (dst-image im-image))

(cffi:defcfun ("imProcessMirror" %im-process-mirror) :int
  "Mirror the image in a horizontal flip. Swap columns. Images must be of the
same type and size. Can be done in-place. Returns zero if the counter
aborted."
  (src-image im-image)
  (dst-image im-image))

(cffi:defcfun ("imProcessFlip" %im-process-flip) :int
  "Apply a vertical flip. Swap lines. Images must be of the same type and
size. Can be done in-place. Returns zero if the counter aborted."
  (src-image im-image)
  (dst-image im-image))

(cffi:defcfun ("imProcessRadial" %im-process-radial) :int
  "Apply a radial distortion using the given interpolation order (see
imProcessResize). Images must be of the same type and size. Returns zero
if the counter aborted."
  (src-image im-image)
  (dst-image im-image)
  (k1 :double)
  (order :int))

(cffi:defcfun ("imProcessLensDistort" %im-process-lens-distort) :int
  "Apply a lens distortion correction using the given interpolation order
(see imProcessResize). a, b, and c are the lens parameters. Images must be
of the same type and size. Returns zero if the counter aborted."
  (src-image im-image)
  (dst-image im-image)
  (a :double)
  (b :double)
  (c :double)
  (order :int))

(cffi:defcfun ("imProcessSwirl" %im-process-swirl) :int
  "Apply a swirl distortion using the given interpolation order (see
imProcessResize). Images must be of the same type and size. Returns zero
if the counter aborted."
  (src-image im-image)
  (dst-image im-image)
  (k1 :double)
  (order :int))

(cffi:defcfun ("imProcessInterlaceSplit" %im-process-interlace-split) :int
  "Split the image in two images, one containing the odd lines and other
containing the even lines. Images must be of the same type. Height of the
output images must be half the height of the input image. If the height of
the input image is odd then the first image must have height equals to
half+1. Returns zero if the counter aborted."
  (src-image im-image)
  (dst-image1 im-image)
  (dst-image2 im-image))

(cffi:defcfun ("imProcessGrayMorphConvolve" %im-process-gray-morph-convolve) :int
  "Base gray morphology convolution. Supports all data types except complex.
Can be applied on color images. Kernel is always IM_INT. Use kernel size
odd for better results. Use -1 for don't care positions in kernel. Kernel
values are added to image values, then you can use the maximum or the
minimum within the kernel area. No border extensions are used. All the
gray morphology operations use this function. Returns zero if the counter
aborted."
  (src-image im-image)
  (dst-image im-image)
  (kernel im-image)
  (ismax :int))

(cffi:defcfun ("imProcessGrayMorphErode" %im-process-gray-morph-erode) :int
  "Gray morphology convolution with a kernel full of \"0\"s and use minimum
value."
  (src-image im-image)
  (dst-image im-image)
  (kernel-size :int))

(cffi:defcfun ("imProcessGrayMorphDilate" %im-process-gray-morph-dilate) :int
  "Gray morphology convolution with a kernel full of \"0\"s and use maximum
value. Returns zero if the counter aborted."
  (src-image im-image)
  (dst-image im-image)
  (kernel-size :int))

(cffi:defcfun ("imProcessGrayMorphOpen" %im-process-gray-morph-open) :int
  "Erode+Dilate. Returns zero if the counter aborted."
  (src-image im-image)
  (dst-image im-image)
  (kernel-size :int))

(cffi:defcfun ("imProcessGrayMorphClose" %im-process-gray-morph-close) :int
  "Dilate+Erode. Returns zero if the counter aborted."
  (src-image im-image)
  (dst-image im-image)
  (kernel-size :int))

(cffi:defcfun ("imProcessGrayMorphTopHat" %im-process-gray-morph-top-hat) :int
  "Open+Difference. Returns zero if the counter aborted."
  (src-image im-image)
  (dst-image im-image)
  (kernel-size :int))

(cffi:defcfun ("imProcessGrayMorphWell" %im-process-gray-morph-well) :int
  "Close+Difference. Returns zero if the counter aborted."
  (src-image im-image)
  (dst-image im-image)
  (kernel-size :int))

(cffi:defcfun ("imProcessGrayMorphGradient" %im-process-gray-morph-gradient) :int
  "Difference(Erode, Dilate). Returns zero if the counter aborted."
  (src-image im-image)
  (dst-image im-image)
  (kernel-size :int))

(cffi:defcfun ("imProcessBinMorphConvolve" %im-process-bin-morph-convolve) :int
  "Base binary morphology convolution. Images are all IM_BINARY. Kernel is
IM_INT, but values can be only 1, 0 or -1. Use kernel size odd for better
results. Hit white means hit=1 and miss=0, or else hit=0 and miss=1. Use
-1 for don't care positions in kernel. Kernel values are simply compared
with image values. The operation can be repeated by a number of
iterations. The border is zero extended. Almost all the binary morphology
operations use this function. Returns zero if the counter aborted."
  (src-image im-image)
  (dst-image im-image)
  (kernel im-image)
  (hit-white :int)
  (iter :int))

(cffi:defcfun ("imProcessBinMorphErode" %im-process-bin-morph-erode) :int
  "Binary morphology convolution with a kernel full of \"1\"s and hit white.
Returns zero if the counter aborted."
  (src-image im-image)
  (dst-image im-image)
  (kernel-size :int)
  (iter :int))

(cffi:defcfun ("imProcessBinMorphDilate" %im-process-bin-morph-dilate) :int
  "Binary morphology convolution with a kernel full of \"0\"s and hit black.
Returns zero if the counter aborted."
  (src-image im-image)
  (dst-image im-image)
  (kernel-size :int)
  (iter :int))

(cffi:defcfun ("imProcessBinMorphOpen" %im-process-bin-morph-open) :int
  "Erode+Dilate. When iteration is more than one it means
Erode+Erode+Erode+...+Dilate+Dilate+Dilate+... Returns zero if the counter
aborted."
  (src-image im-image)
  (dst-image im-image)
  (kernel-size :int)
  (iter :int))

(cffi:defcfun ("imProcessBinMorphClose" %im-process-bin-morph-close) :int
  "Dilate+Erode. Returns zero if the counter aborted."
  (src-image im-image)
  (dst-image im-image)
  (kernel-size :int)
  (iter :int))

(cffi:defcfun ("imProcessBinMorphOutline" %im-process-bin-morph-outline) :int
  "Erode+Difference. The difference from the source image is applied only
once. Returns zero if the counter aborted."
  (src-image im-image)
  (dst-image im-image)
  (kernel-size :int)
  (iter :int))

(cffi:defcfun ("imProcessBinThinZhangSuen" %im-process-bin-thin-zhang-suen) :int
  "Thins the supplied binary image using Zhang-Suen thinning algorithm.
Reference: Rosetta Code
https://rosettacode.org/wiki/Zhang-Suen_thinning_algorithm Not using
OpenMP when enabled. Returns zero if the counter aborted (counter is
approximate). (since 3.14)"
  (src-image im-image)
  (dst-image im-image))

(cffi:defcfun ("imProcessBinThinNhMaps" %im-process-bin-thin-nh-maps) :int
  "Thins the supplied binary image using Rosenfeld's parallel thinning
algorithm. Reference: \"Efficient Binary Image Thinning using Neighborhood
Maps\" by Joseph M. Cychosz, 3ksnn64@ecn.purdue.edu in \"Graphics Gems
IV\", Academic Press, 1994 Not using OpenMP when enabled. Returns zero if
the counter aborted (counter is approximate). (renamed in 3.14)"
  (src-image im-image)
  (dst-image im-image))

(cffi:defcfun ("imProcessMedianConvolve" %im-process-median-convolve) :int
  "Rank convolution using the median value. Returns zero if the counter
aborted. Supports all data types except complex. Can be applied on color
images."
  (src-image im-image)
  (dst-image im-image)
  (kernel-size :int))

(cffi:defcfun ("imProcessRangeConvolve" %im-process-range-convolve) :int
  "Rank convolution using (maximum-minimum) value. Returns zero if the
counter aborted. Supports all data types except complex. Can be applied on
color images."
  (src-image im-image)
  (dst-image im-image)
  (kernel-size :int))

(cffi:defcfun ("imProcessRankClosestConvolve" %im-process-rank-closest-convolve) :int
  "Rank convolution using the closest maximum or minimum value. Returns zero
if the counter aborted. Supports all data types except complex. Can be
applied on color images."
  (src-image im-image)
  (dst-image im-image)
  (kernel-size :int))

(cffi:defcfun ("imProcessRankMaxConvolve" %im-process-rank-max-convolve) :int
  "Rank convolution using the maximum value. Returns zero if the counter
aborted. Supports all data types except complex. Can be applied on color
images."
  (src-image im-image)
  (dst-image im-image)
  (kernel-size :int))

(cffi:defcfun ("imProcessRankMinConvolve" %im-process-rank-min-convolve) :int
  "Rank convolution using the minimum value. Returns zero if the counter
aborted. Supports all data types except complex. Can be applied on color
images."
  (src-image im-image)
  (dst-image im-image)
  (kernel-size :int))

(cffi:defcfun ("imProcessRangeContrastThreshold" %im-process-range-contrast-threshold) :int
  "Threshold using a rank convolution with a range contrast function.
Supports all integer IM_GRAY images as source, and IM_BINARY as target.
Local variable threshold by the method of Bernsen. Extracted from XITE,
Copyright 1991, Blab, UiO http://www.ifi.uio.no/~blab/Software/Xite/
Returns zero if the counter aborted."
  (src-image im-image)
  (dst-image im-image)
  (kernel-size :int)
  (min-range :int))

(cffi:defcfun ("imProcessLocalMaxThreshold" %im-process-local-max-threshold) :int
  "Threshold using a rank convolution with a local max function. Returns zero
if the counter aborted. Supports all integer IM_GRAY images as source, and
IM_BINARY as target."
  (src-image im-image)
  (dst-image im-image)
  (kernel-size :int)
  (min-level :int))

(cffi:defcfun ("imProcessConvolve" %im-process-convolve) :int
  "Base Convolution with a kernel. Kernel can be IM_INT or IM_FLOAT, but
always IM_GRAY. Use kernel size odd for better results. Supports all data
types. The border is mirrored. Returns zero if the counter aborted. Most
of the convolutions use this function."
  (src-image im-image)
  (dst-image im-image)
  (kernel im-image))

(cffi:defcfun ("imProcessConvolveSep" %im-process-convolve-sep) :int
  "Base convolution when the kernel is separable. Only the first line and the
first column will be used. Returns zero if the counter aborted."
  (src-image im-image)
  (dst-image im-image)
  (kernel im-image))

(cffi:defcfun ("imProcessConvolveDual" %im-process-convolve-dual) :int
  "Base Convolution with two kernels. The result is the magnitude of the
result of each convolution. Kernel can be IM_INT or IM_FLOAT, but always
IM_GRAY. Use kernel size odd for better results. Supports all data types.
The border is mirrored. Returns zero if the counter aborted. Most of the
convolutions use this function."
  (src-image im-image)
  (dst-image im-image)
  (kernel1 im-image)
  (kernel2 im-image))

(cffi:defcfun ("imProcessConvolveRep" %im-process-convolve-rep) :int
  "Repeats the convolution a number of times. Returns zero if the counter
aborted."
  (src-image im-image)
  (dst-image im-image)
  (kernel im-image)
  (count :int))

(cffi:defcfun ("imProcessCompassConvolve" %im-process-compass-convolve) :int
  "Convolve with a kernel rotating it 8 times and getting the absolute
maximum value. Kernel must be square. The rotation is implemented only for
kernel sizes 3x3, 5x5 and 7x7. Supports all data types except complex.
Returns zero if the counter aborted."
  (src-image im-image)
  (dst-image im-image)
  (kernel im-image))

(cffi:defcfun ("imProcessRotateKernel" %im-process-rotate-kernel) :void
  "Utility function to rotate a kernel one time."
  (kernel im-image))

(cffi:defcfun ("imProcessDiffOfGaussianConvolve" %im-process-diff-of-gaussian-convolve) :int
  "Difference(Gaussian1, Gaussian2). Supports all data types, but if source
is IM_BYTE or IM_USHORT target image must be of type IM_INT. Returns zero
if the counter aborted."
  (src-image im-image)
  (dst-image im-image)
  (stddev1 :double)
  (stddev2 :double))

(cffi:defcfun ("imProcessLapOfGaussianConvolve" %im-process-lap-of-gaussian-convolve) :int
  "Convolution with a laplacian of a gaussian kernel. Supports all data
types, but if source is IM_BYTE or IM_USHORT target image must be of type
IM_INT. Returns zero if the counter aborted."
  (src-image im-image)
  (dst-image im-image)
  (stddev :double))

(cffi:defcfun ("imProcessMeanConvolve" %im-process-mean-convolve) :int
  "Convolution with a kernel full of \"1\"s inside a circle. Supports all
data types. Returns zero if the counter aborted."
  (src-image im-image)
  (dst-image im-image)
  (kernel-size :int))

(cffi:defcfun ("imProcessGaussianConvolve" %im-process-gaussian-convolve) :int
  "Convolution with a gaussian kernel with floating point values. If sdtdev
is negative its magnitude will be used as the kernel size. Supports all
data types. Returns zero if the counter aborted."
  (src-image im-image)
  (dst-image im-image)
  (stddev :double))

(cffi:defcfun ("imProcessBarlettConvolve" %im-process-barlett-convolve) :int
  "Convolution with a barlett kernel. Supports all data types. Returns zero
if the counter aborted."
  (src-image im-image)
  (dst-image im-image)
  (kernel-size :int))

(cffi:defcfun ("imProcessSobelConvolve" %im-process-sobel-convolve) :int
  "Magnitude of the sobel convolution. Supports all data types. Returns zero
if the counter aborted."
  (src-image im-image)
  (dst-image im-image))

(cffi:defcfun ("imProcessPrewittConvolve" %im-process-prewitt-convolve) :int
  "Magnitude of the prewitt convolution. Supports all data types. Returns
zero if the counter aborted."
  (src-image im-image)
  (dst-image im-image))

(cffi:defcfun ("imProcessSplineEdgeConvolve" %im-process-spline-edge-convolve) :int
  "Spline edge dectection. Supports all data types. Returns zero if the
counter aborted."
  (src-image im-image)
  (dst-image im-image))

(cffi:defcfun ("imProcessZeroCrossing" %im-process-zero-crossing) :int
  "Finds the zero crossings of IM_SHORT, IM_INT, IM_FLOAT and IM_DOUBLE
images. Crossings are marked with non zero values indicating the intensity
of the edge. It is usually used after a second derivative, laplace.
Extracted from XITE, Copyright 1991, Blab, UiO
http://www.ifi.uio.no/~blab/Software/Xite/ Returns zero if the counter
aborted."
  (src-image im-image)
  (dst-image im-image))

(cffi:defcfun ("imProcessCanny" %im-process-canny) :int
  "First part of the Canny edge detector. Includes the gaussian filtering and
the nonmax suppression. After using this you could apply a Hysteresis
Threshold, see imProcessHysteresisThreshold. Image must be
IM_BYTE/IM_GRAY. Returns zero if the counter aborted. Implementation from
the book:"
  (src-image im-image)
  (dst-image im-image)
  (stddev :double))

(cffi:defcfun ("imGaussianStdDev2KernelSize" %im-gaussian-std-dev2-kernel-size) :int
  "Calculates the kernel size given the standard deviation. If sdtdev is
negative its magnitude will be used as the kernel size."
  (stddev :double))

(cffi:defcfun ("imGaussianKernelSize2StdDev" %im-gaussian-kernel-size2-std-dev) :double
  "Calculates the standard deviation given the kernel size."
  (kernel-size :int))

(cffi:defcfun ("imProcessUnsharp" %im-process-unsharp) :int
  "Edge enhancement using Unsharp mask. stddev control the gaussian filter,
amount controls how much the edges will enhance the image (0<amount<1),
and threshold controls which edges will be considered, it compares to
twice of the absolute size of the edge. Although very similar to
imProcessSharp, produces better results."
  (src-image im-image)
  (dst-image im-image)
  (stddev :double)
  (amount :double)
  (threshold :double))

(cffi:defcfun ("imProcessSharp" %im-process-sharp) :int
  "Edge enhancement using Laplacian8 mask. amount controls how much the edges
will enhance the image (0<amount<1), and threshold controls which edges
will be considered, it compares to twice of the absolute size of the edge.
Returns zero if the counter aborted."
  (src-image im-image)
  (dst-image im-image)
  (amount :double)
  (threshold :double))

(cffi:defcfun ("imProcessSharpKernel" %im-process-sharp-kernel) :int
  "Edge enhancement using a given kernel. If kernel has all positive values,
then the unsharp technique is used, else sharp is used. amount controls
how much the edges will enhance the image (0<amount<1), and threshold
controls which edges will be considered, it compares to twice of the
absolute size of the edge. Returns zero if the counter aborted."
  (src-image im-image)
  (kernel im-image)
  (dst-image im-image)
  (amount :double)
  (threshold :double))
