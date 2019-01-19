(defpackage #:im-convert
  (:use #:common-lisp)
  (:export #:+gamma-linear+
	   #:+gamma-log-lite+
	   #:+gamma-log-heavy+
	   #:+gamma-exp-lite+
	   #:+gamma-exp-heavy+
	   #:to-data-type
	   #:to-color-space
	   #:to-bitmap
	   #:to-packing
	   #:map-to-rgb))

(in-package #:im-convert)

(defconstant +gamma-linear+ 0.0)
(defconstant +gamma-log-lite+ -10.0)
(defconstant +gamma-log-heavy+ -1000.0)
(defconstant +gamma-exp-lite+ 2.0)
(defconstant +gamma-exp-heavy+ 7.0)

(defun to-data-type
    (src-im-image dst-im-image complex-to-real gamma absolute-p convert-cast-mode)
  "Changes the image data type, using a complex to real conversion, a
gamma factor, and an absolute mode (modulus).

When demoting the data type the function will scan source for min/max
values or use fixed values (CONVERT-CAST-MODE) to scale the result
according to the target range. Except complex to real that will use
only the COMPLEX-TO-REAL conversion. Images must be of the same size
and color mode. If data type is the same nothing is done.

Signals IM-ERROR on error."
  (im::maybe-error
   (im-cffi::%im-convert-data-type
    src-im-image
    dst-im-image
    complex-to-real
    gamma
    absolute-p
    convert-cast-mode)))

(defun to-color-space (src-im-image dst-im-image)
  "Converts one color space to another. 

Images must be of the same size and data type. If color mode is the
same nothing is done. CMYK can be converted to RGB only, and it is a
very simple conversion. All colors can be converted to Binary, the non
zero gray values are converted to 1. RGB to Map uses the median cut
implementation from the free IJG JPEG software, copyright Thomas
G. Lane.

Alpha channel is considered and transparency attributes are converted
to alpha channel.  All other color space conversions assume sRGB and
CIE definitions, see Color Manipulation.  

Signals IM-ERROR on error."
  (im::maybe-error
   (im-cffi::%im-convert-color-space
    src-im-image
    dst-im-image)))

(defun to-packing
    (src-data-ptr dst-data-ptr width height src-depth dst-depth data-type src-packed-p)
  "Changes the packing of the data buffer. 

Both must have the same width, height and data-type. It can be used to
copy data even if depth = 1. Typically used in OpenGL data
conversions."
  (im-cffi::%im-convert-packing
   src-data-ptr
   dst-data-ptr
   width
   height
   src-depth
   dst-depth
   data-type
   src-packed-p))

(defun map-to-rgb (data-ptr count depth packed-p palette)
  "Changes in-place a MAP data into a RGB data. The data must have
room for the RGB image. DEPTH can be 3 or 4. COUNT = width *
height. Typically used in OpenGL data conversions."
  (im-cffi::%im-convert-map-to-rgb
   data-ptr
   count
   depth
   packed-p
   (im-palette:data palette)
   (im-palette:count palette)))
