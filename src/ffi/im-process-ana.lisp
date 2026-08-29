;;;; src/ffi/im-process-ana.lisp — DRAFTED by tools/gen-bindings.lisp.
;;;;
;;;; Source: im_process_ana.h
;;;; Hand corrections below this line are expected and are kept;
;;;; re-run the generator into a clean tree and diff.

(in-package #:im.ffi)

(cffi:defcfun ("imCalcRMSError" %im-calc-rms-error) :int
  "Calculates the RMS error between two images (Root Mean Square Error).
Returns zero if the counter aborted."
  (image1 im-image)
  (image2 im-image)
  (rmserror :pointer))

(cffi:defcfun ("imCalcSNR" %im-calc-snr) :int
  "Calculates the SNR of an image and its noise (Signal Noise Ratio). Returns
zero if the counter aborted."
  (src-image im-image)
  (noise-image im-image)
  (snr :pointer))

(cffi:defcfun ("imCalcCountColors" %im-calc-count-colors) :int
  "Count the number of different colors in an image. Image must be IM_BYTE,
but can has all color spaces except IM_CMYK. Data type can be also
IM_SHORT or IM_USHORT if color space is IM_GRAY, IM_BINARY or IM_MAP. Not
using OpenMP when enabled, when color space depth is greater than 1.
Returns zero if the counter aborted."
  (image im-image)
  (count :pointer))

(cffi:defcfun ("imCalcGrayHistogram" %im-calc-gray-histogram) :int
  "Calculates the gray histogram of an image. Image must be (IM_BYTE,
IM_SHORT or IM_USHORT)/(IM_RGB, IM_GRAY, IM_BINARY or IM_MAP). If the
image is IM_RGB then the histogram of the luma component is calculated.
Histogram is always 256 or 65536 positions long. When cumulative is
different from zero it calculates the cumulative histogram. Returns zero
if the counter aborted."
  (image im-image)
  (histo :pointer)
  (cumulative :int))

(cffi:defcfun ("imCalcHistogram" %im-calc-histogram) :int
  "Calculates the histogram of an image plane. Image can be IM_BYTE, IM_SHORT
or IM_USHORT. Histogram is always 256 or 65536 positions long. Where plane
is the depth plane to calculate the histogram. When cumulative is
different from zero it calculates the cumulative histogram. Returns zero
if the counter aborted. The returned table is zero indexed."
  (image im-image)
  (histo :pointer)
  (plane :int)
  (cumulative :int))

(cffi:defcfun ("imCalcByteHistogram" %im-calc-byte-histogram) :void
  "Calculates the histogram of a IM_BYTE data. Histogram is always 256
positions long. When cumulative is different from zero it calculates the
cumulative histogram. Not available in Lua."
  (data :pointer)
  (count :int)
  (histo :pointer)
  (cumulative :int))

(cffi:defcfun ("imCalcUShortHistogram" %im-calc-ushort-histogram) :void
  "Calculates the histogram of a IM_USHORT data. Histogram is always 65536
positions long. When cumulative is different from zero it calculates the
cumulative histogram. Not available in Lua."
  (data :pointer)
  (count :int)
  (histo :pointer)
  (cumulative :int))

(cffi:defcfun ("imCalcShortHistogram" %im-calc-short-histogram) :void
  "Calculates the histogram of a IM_SHORT data. Histogram is always 65536
positions long. Zero is located at 32768 index. When cumulative is
different from zero it calculates the cumulative histogram. Not available
in Lua."
  (data :pointer)
  (count :int)
  (histo :pointer)
  (cumulative :int))

(cffi:defcfun ("imHistogramNew" %im-histogram-new) :pointer
  "Allocates an histogram data based on the image data type. Data type can be
IM_BYTE, IM_SHORT or IM_USHORT. Not available in Lua."
  (data-type :int)
  (hcount :pointer))

(cffi:defcfun ("imHistogramRelease" %im-histogram-release) :void
  "Releases the histogram data. Not available in Lua."
  (histo :pointer))

(cffi:defcfun ("imHistogramShift" %im-histogram-shift) :int
  "Short data type stores the histogram values of negative indexes starting
at 0. So the real level is obtained by shifting the zero based index. Not
available in Lua."
  (data-type :int))

(cffi:defcfun ("imHistogramCount" %im-histogram-count) :int
  "Returns the histogram size based on the image data type. For IM_IM_USHORT
and IM_SHORT returns 65536 for others returns 256. Not available in Lua."
  (data-type :int))

(cffi:defcfun ("imCalcImageStatistics" %im-calc-image-statistics) :int
  "Calculates the statistics about the image data. There is one stats for
each depth plane. For ex: stats[0]=red stats, stats[0]=green stats, ...
Supports all data types except complex. Returns zero if the counter
aborted. Table contains the following fields: max, min, positive,
negative, zeros, mean, stddev. If image depth > 1 then table contains
several tables with the previous fields, one for each plane, starting at
0. The same as the imStats structure."
  (image im-image)
  (stats :pointer))

(cffi:defcfun ("imCalcHistogramStatistics" %im-calc-histogram-statistics) :int
  "Calculates the statistics about the image histogram data. There is one
stats for each depth plane. For ex: stats[0]=red stats, stats[0]=green
stats, ... Only IM_BYTE, IM_SHORT and IM_USHORT images are supported.
Returns zero if the counter aborted."
  (image im-image)
  (stats :pointer))

(cffi:defcfun ("imCalcHistoImageStatistics" %im-calc-histo-image-statistics) :int
  "Calculates some extra statistics about the image histogram data. There is
one stats for each depth plane. Only IM_BYTE, IM_SHORT and IM_USHORT
images are supported. mode will be -1 if more than one max is found.
Returns zero if the counter aborted."
  (image im-image)
  (median :pointer)
  (mode :pointer))

(cffi:defcfun ("imCalcPercentMinMax" %im-calc-percent-min-max) :int
  "Calculates the minimum and maximum levels ignoring a given percentage of
the histogram count. Used by imProcessExpandHistogram. Only IM_BYTE,
IM_SHORT and IM_USHORT images are supported. Returns zero if the counter
aborted."
  (image im-image)
  (percent :double)
  (ignore-zero :int)
  (min :pointer)
  (max :pointer))

(cffi:defcfun ("imAnalyzeFindRegions" %im-analyze-find-regions) :int
  "Find white regions in binary image. Result is IM_GRAY/IM_USHORT type.
Regions can be 4 connected or 8 connected. The number of regions found is
returned in region_count. Background is marked as 0, and it is not
included in count. Regions touching the border are considered only if
touch_border=1. Not using OpenMP when enabled. Returns zero if the counter
aborted."
  (src-image im-image)
  (dst-image im-image)
  (connect :int)
  (touch-border :int)
  (region-count :pointer))

(cffi:defcfun ("imAnalyzeMeasureArea" %im-analyze-measure-area) :int
  "Measure the actual area of all regions. Holes are not included. This is
the number of pixels of each region. Source image is IM_GRAY/IM_USHORT
type (the result of imAnalyzeFindRegions). area has size the number of
regions. Returns zero if the counter aborted. The returned table is zero
indexed."
  (image im-image)
  (area :pointer)
  (region-count :int))

(cffi:defcfun ("imAnalyzeMeasurePerimArea" %im-analyze-measure-perim-area) :int
  "Measure the polygonal area limited by the perimeter line of all regions.
Holes are not included. Notice that some regions may have polygonal area
zero. Source image is IM_GRAY/IM_USHORT type (the result of
imAnalyzeFindRegions). perimarea has size the number of regions. Returns
zero if the counter aborted. The returned table is zero indexed."
  (image im-image)
  (perimarea :pointer)
  (region-count :int))

(cffi:defcfun ("imAnalyzeMeasureCentroid" %im-analyze-measure-centroid) :int
  "Calculate the centroid position of all regions. Holes are not included.
Source image is IM_GRAY/IM_USHORT type (the result of
imAnalyzeFindRegions). area, cx and cy have size the number of regions. If
area is NULL will be internally calculated. Returns zero if the counter
aborted. The returned tables are zero indexed."
  (image im-image)
  (area :pointer)
  (region-count :int)
  (cx :pointer)
  (cy :pointer))

(cffi:defcfun ("imAnalyzeMeasurePrincipalAxis" %im-analyze-measure-principal-axis) :int
  "Calculate the principal major axis slope of all regions. Source image is
IM_GRAY/IM_USHORT type (the result of imAnalyzeFindRegions). data has size
the number of regions. If area or centroid are NULL will be internally
calculated. Principal (major and minor) axes are defined to be those axes
that pass through the centroid, about which the moment of inertia of the
region is, respectively maximal or minimal. Partially using OpenMP when
enabled. Returns zero if the counter aborted. The returned tables are zero
indexed."
  (image im-image)
  (area :pointer)
  (cx :pointer)
  (cy :pointer)
  (region-count :int)
  (major-slope :pointer)
  (major-length :pointer)
  (minor-slope :pointer)
  (minor-length :pointer))

(cffi:defcfun ("imAnalyzeMeasureHoles" %im-analyze-measure-holes) :int
  "Measure the number of holes of all regions. Optionally computes the holes
area and holes perimeter of all regions. Source image is IM_GRAY/IM_USHORT
type (the result of imAnalyzeFindRegions). count, area and perim has size
the number of regions, if some is NULL it will be not calculated. Not
using OpenMP when enabled. Returns zero if the counter aborted. The
returned tables are zero indexed."
  (image im-image)
  (connect :int)
  (region-count :int)
  (holes-count :pointer)
  (holes-area :pointer)
  (holes-perim :pointer))

(cffi:defcfun ("imAnalyzeMeasurePerimeter" %im-analyze-measure-perimeter) :int
  "Measure the total perimeter of all regions (external and internal). Source
image is IM_GRAY/IM_USHORT type (the result of imAnalyzeFindRegions). It
uses a half-pixel inter distance for 8 neighbors in a perimeter of a 4
connected region. This function can also be used to measure line length.
perim has size the number of regions. Returns zero if the counter aborted."
  (image im-image)
  (perim :pointer)
  (region-count :int))

(cffi:defcfun ("imProcessPerimeterLine" %im-process-perimeter-line) :int
  "Isolates the perimeter line of gray integer images. Background is defined
as being black (0). It just checks if at least one of the 4 connected
neighbors is non zero. Image borders are extended with zeros. Returns zero
if the counter aborted."
  (src-image im-image)
  (dst-image im-image))

(cffi:defcfun ("imProcessRemoveByArea" %im-process-remove-by-area) :int
  "Eliminates regions that have area size outside or inside the given
interval. Source and target are a binary images. Regions can be 4
connected or 8 connected. Can be done in-place. end_size can be zero to
indicate no upper limit or an area with width*height size. When searching
inside the region the limits are inclusive (<= size >=), when searching
outside the limits are exclusive (> size <)."
  (src-image im-image)
  (dst-image im-image)
  (connect :int)
  (start-size :int)
  (end-size :int)
  (inside :int))

(cffi:defcfun ("imProcessFillHoles" %im-process-fill-holes) :int
  "Fill holes inside white regions. Source and target are a binary images.
Regions can be 4 connected or 8 connected. Can be done in-place. Returns
zero if the counter aborted."
  (src-image im-image)
  (dst-image im-image)
  (connect :int))
