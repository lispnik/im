;;;; src/ffi/im-process-glo.lisp — DRAFTED by tools/gen-bindings.lisp.
;;;;
;;;; Source: im_process_glo.h
;;;; Hand corrections below this line are expected and are kept;
;;;; re-run the generator into a clean tree and diff.

(in-package #:im.ffi)

(cffi:defcfun ("imProcessHoughLines" %im-process-hough-lines) :int
  "Hough Lines Transform. It will detect white lines in a black background.
So the source image must be a IM_BINARY image with the white lines of
interest enhanced. The better the threshold with the white lines the
better the line detection. The target image must have IM_GRAY, IM_INT,
hg_width=180, hg_height=2*rmax+1, where rmax is the image diagonal/2 (rmax
= srqrt(width*width + height*height)). The hough transform defines
\"cos(theta) * X + sin(theta) * Y = rho\" and the parameters are in the
interval: theta = \"0 .. 179\", rho = \"-hg_height/2 .. hg_height/2\" .
Where rho is the perpendicular distance from the center of the image and
theta the angle with the normal. So do not confuse theta with the line
angle, they are perpendicular. Returns zero if the counter aborted.
Inspired from ideas in XITE, Copyright 1991, Blab, UiO
http://www.ifi.uio.no/~blab/Software/Xite/ Not using OpenMP when enabled."
  (src-image im-image)
  (dst-image im-image))

(cffi:defcfun ("imProcessHoughLinesDraw" %im-process-hough-lines-draw) :int
  "Draw detected hough lines. The source and target images can be IM_MAP,
IM_GRAY or IM_RGB, with data type IM_BYTE. Can be done in-place. If the
hough transform is not NULL, then the hough points are filtered to include
only lines that are significally different from each other. The hough
image is the hough transform image, but it is optional and can be NULL. If
not NULL then it will be used to filter lines that are very similar. The
hough points image is a hough transform image that was thresholded to a
IM_BINARY image, usually using a Local Max threshold operation (see
imProcessLocalMaxThreshold). Again the better the threshold the better the
results. The detected lines will be drawn using a red color. If the target
image is IM_GRAY, it will be changed to IM_MAP. If the target image is
IM_RGB, then only the red plane will be changed. Returns the number of
detected lines. Not using OpenMP when enabled."
  (src-image im-image)
  (hough im-image)
  (hough-points im-image)
  (dst-image im-image))

(cffi:defcfun ("imProcessCrossCorrelation" %im-process-cross-correlation) :void
  "Calculates the Cross Correlation in the frequency domain. CrossCorr(a,b) =
IFFT(Conj(FFT(a))*FFT(b)) Images must be of the same size and only target
image must be of type complex."
  (src-image1 im-image)
  (src-image2 im-image)
  (dst-image im-image))

(cffi:defcfun ("imProcessAutoCorrelation" %im-process-auto-correlation) :void
  "Calculates the Auto Correlation in the frequency domain. Uses the cross
correlation. Images must be of the same size and only target image must be
of type complex."
  (src-image im-image)
  (dst-image im-image))

(cffi:defcfun ("imProcessDistanceTransform" %im-process-distance-transform) :void
  "Calculates the Distance Transform of a binary image using an aproximation
of the euclidian distance. Each white pixel in the binary image is
assigned a value equal to its distance from the nearest black pixel. Uses
a two-pass algorithm incrementally calculating the distance. Source image
must be IM_BINARY, target must be IM_FLOAT or IM_DOUBLE."
  (src-image im-image)
  (dst-image im-image))

(cffi:defcfun ("imProcessRegionalMaximum" %im-process-regional-maximum) :void
  "Marks all the regional maximum of the distance transform. source must be
IM_GRAY+IM_FLOAT/IM_DOUBLE, target must be IM_BINARY. We consider maximum
all connected pixel values that have smaller pixel values around it."
  (src-image im-image)
  (dst-image im-image))

(cffi:defcfun ("imProcessFFT" %im-process-fft) :void
  "Forward FFT. The result has its lowest frequency at the center of the
image. This is an unnormalized fft. Images must be of the same size.
Target image must be of type complex."
  (src-image im-image)
  (dst-image im-image))

(cffi:defcfun ("imProcessIFFT" %im-process-ifft) :void
  "Inverse FFT. The image has its lowest frequency restored to the origin
before the transform. The result is normalized by (width*height). Images
must be of the same size and both must be of type complex."
  (src-image im-image)
  (dst-image im-image))

(cffi:defcfun ("imProcessFFTraw" %im-process-fft-raw) :void
  "Raw in-place FFT (forward or inverse). The lowest frequency can be
centered after forward, or can be restored to the origin before inverse.
The result can be normalized after the transform by sqrt(w*h) [1] or by
(w*h) [2], or left unnormalized [0]. Images must be of the same size and
both must be of type complex."
  (image im-image)
  (inverse :int)
  (center :int)
  (normalize :int))

(cffi:defcfun ("imProcessSwapQuadrants" %im-process-swap-quadrants) :void
  "Auxiliary function for the raw FFT. This is the function used internally
to change the lowest frequency position in the image. If the image size
has even dimensions the flag \"center2origin\" is useless. But if it is
odd, you must specify if its from center to origin (usually used before
inverse) or from origin to center (usually used after forward). Notice
that this function is used for images in the the frequency domain. Image
type must be complex."
  (image im-image)
  (center2origin :int))

(cffi:defcfun ("imProcessOpenMPSetMinCount" %im-process-open-mp-set-min-count) :int
  "Sets the minimum number of iterations to split into threads. Default value
is 250000, or an image with 500x500. Returns the previous value."
  (min-count :int))

(cffi:defcfun ("imProcessOpenMPSetNumThreads" %im-process-open-mp-set-num-threads) :int
  "Sets the number of threads. Does nothing if OpenMP is not enabled. Returns
the previous value."
  (count :int))
