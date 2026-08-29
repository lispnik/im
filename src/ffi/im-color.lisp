;;;; src/ffi/im-color.lisp — DRAFTED by tools/gen-bindings.lisp.
;;;;
;;;; Source: im_color.h, im_colorhsi.h
;;;; Hand corrections below this line are expected and are kept;
;;;; re-run the generator into a clean tree and diff.

(in-package #:im.ffi)

(cffi:defcfun ("imColorRGB2HSI" %im-color-rgb2-hsi) :void
  "Converts from RGB to HSI."
  (r :double)
  (g :double)
  (b :double)
  (h :pointer)
  (s :pointer)
  (i :pointer))

(cffi:defcfun ("imColorRGB2HSIbyte" %im-color-rgb2-hs-ibyte) :void
  "Converts from RGB (byte) to HSI."
  (r :unsigned-char)
  (g :unsigned-char)
  (b :unsigned-char)
  (h :pointer)
  (s :pointer)
  (i :pointer))

(cffi:defcfun ("imColorHSI2RGB" %im-color-hsi2-rgb) :void
  "Converts from HSI to RGB."
  (h :double)
  (s :double)
  (i :double)
  (r :pointer)
  (g :pointer)
  (b :pointer))

(cffi:defcfun ("imColorHSI2RGBbyte" %im-color-hsi2-rg-bbyte) :void
  "Converts from HSI to RGB (byte)."
  (h :double)
  (s :double)
  (i :double)
  (r :pointer)
  (g :pointer)
  (b :pointer))

(cffi:defcfun ("imColorHue" %im-color-hue) :double
  "Returns just the hue of the color RGB."
  (r :double)
  (g :double)
  (b :double))

(cffi:defcfun ("imColorHueByte" %im-color-hue-byte) :double
  "Returns just the hue of the color RGB (byte)."
  (r :unsigned-char)
  (g :unsigned-char)
  (b :unsigned-char))

(cffi:defcfun ("imColorIntensity" %im-color-intensity) :double
  "Returns just the intensity of the color RGB."
  (r :double)
  (g :double)
  (b :double))

(cffi:defcfun ("imColorIntensityByte" %im-color-intensity-byte) :double
  "Returns just the intensity of the color RGB (byte)."
  (r :unsigned-char)
  (g :unsigned-char)
  (b :unsigned-char))

(cffi:defcfun ("imColorSaturation" %im-color-saturation) :double
  "Returns just the saturation of the color RGB. Here S is not normalized by
Smax."
  (r :double)
  (g :double)
  (b :double))

(cffi:defcfun ("imColorSaturationByte" %im-color-saturation-byte) :double
  "Returns just the saturation of the color RGB (byte)."
  (r :unsigned-char)
  (g :unsigned-char)
  (b :unsigned-char))

(cffi:defcfun ("imColorHSI_ImaxS" %im-color-hsi_-imax-s) :double
  "Returns I where S is maximum given H (here in radians)."
  (h :double)
  (cos-h :double)
  (sin-h :double))

(cffi:defcfun ("imColorHSI_Smax" %im-color-hsi_-smax) :double
  "Returns maximum S (unnormalized) given I and H (here in radians)."
  (h :double)
  (cos-h :double)
  (sin-h :double)
  (i :double))
