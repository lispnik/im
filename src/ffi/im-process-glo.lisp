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

(cffi:defcfun ("imProcessWatershed" %im-process-watershed) :int
  "Marker-controlled watershed, by Meyer's flooding algorithm. Reads
src_image as a relief map and floods it from the labelled markers, lowest
ground first, so every pixel joins the marker whose water reached it. All
three images must be of the same size and one plane. src_image is IM_GRAY
of any real data type -- LOW values are flooded first, so basins must be
the features of interest; negate the image if they are not. marker_image
and dst_image are IM_GRAY/IM_USHORT, marker_image labelled as
imAnalyzeFindRegions labels, with 0 meaning unmarked. dst_image may be the
same image as marker_image. connect is 4 or 8. When mark_lines is
non-zero, a pixel that two different basins reach at once is left as 0 in
dst_image and belongs to neither, which draws one-pixel watershed lines
between the regions; when it is zero, every pixel is assigned and the
regions meet directly. A region seeded by no marker is never labelled:
this segments the markers given, it does not find them.
imProcessWatershedSegment is the usual way to obtain markers. Not using
OpenMP when enabled -- the flood is inherently sequential. Returns zero if
the counter aborted."
  (src-image im-image)
  (marker-image im-image)
  (dst-image im-image)
  (connect :int)
  (mark-lines :int))

(cffi:defcfun ("imProcessWatershedSegment" %im-process-watershed-segment) :int
  "Separates touching objects in a binary image, and labels them. src_image
is IM_BINARY, dst_image is IM_GRAY/IM_USHORT and receives one label per
object, exactly as imAnalyzeFindRegions would -- so every
imAnalyzeMeasure* function reads the result directly. Both must be the
same size. region_count returns the number of objects found. This is what
imAnalyzeFindRegions cannot do: two objects that touch are one connected
region, and no amount of labelling will make them two. The recipe is
imProcessDistanceTransform to find how deep inside an object each pixel
is, imProcessRegionalMaximum to find the object centres, and a watershed
of the negated distance map seeded from those centres, which splits the
pair along the neck between them. connect is 4 or 8, and applies both to
grouping the markers and to the flood. mark_lines leaves a one-pixel gap
of 0 between objects when non-zero. Objects touching the border are
included. Convex objects of similar size separate cleanly; a strongly
concave object can carry more than one distance maximum and be split in
two, which is this method's characteristic failure and is not detectable
from the output. Returns zero if the counter aborted."
  (src-image im-image)
  (dst-image im-image)
  (connect :int)
  (mark-lines :int)
  (region-count :pointer))

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
