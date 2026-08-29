;;;; src/ffi/im-palette.lisp — DRAFTED by tools/gen-bindings.lisp.
;;;;
;;;; Source: im_palette.h
;;;; Hand corrections below this line are expected and are kept;
;;;; re-run the generator into a clean tree and diff.

(in-package #:im.ffi)

(cffi:defcfun ("imPaletteNew" %im-palette-new) :pointer
  "Allocates memory for the palette data. This ensures allocation and release
in the same module by the correct functions."
  (count :int))

(cffi:defcfun ("imPaletteRelease" %im-palette-release) :void
  "Releases memory for the palette data. This ensures allocation and release
in the same module by the correct functions."
  (palette :pointer))

(cffi:defcfun ("imPaletteDuplicate" %im-palette-duplicate) :pointer
  "Duplicate a palette data using imPaletteNew."
  (palette :pointer)
  (count :int))

(cffi:defcfun ("imPaletteFindNearest" %im-palette-find-nearest) :int
  "Searches for the nearest color on the table and returns the color index if
successful. It looks in all palette entries and finds the minimum
euclidian square distance. If the color matches the given color it returns
immediately. See also colorutl."
  (palette :pointer)
  (palette-count :int)
  (color :long))

(cffi:defcfun ("imPaletteFindColor" %im-palette-find-color) :int
  "Searches for the color on the table and returns the color index if
successful. If the tolerance is 0 search for the exact match in the
palette else search for the first color that fits in the tolerance range.
See also colorutl."
  (palette :pointer)
  (palette-count :int)
  (color :long)
  (tol :unsigned-char))

(cffi:defcfun ("imPaletteGray" %im-palette-gray) :pointer
  "Creates a palette of gray scale values. The colors are arranged from black
to white.")

(cffi:defcfun ("imPaletteRed" %im-palette-red) :pointer
  "Creates a palette of a gradient of red colors. The colors are arranged
from black to pure red.")

(cffi:defcfun ("imPaletteGreen" %im-palette-green) :pointer
  "Creates a palette of a gradient of green colors. The colors are arranged
from black to pure green.")

(cffi:defcfun ("imPaletteBlue" %im-palette-blue) :pointer
  "Creates a palette of a gradient of blue colors. The colors are arranged
from black to pure blue.")

(cffi:defcfun ("imPaletteYellow" %im-palette-yellow) :pointer
  "Creates a palette of a gradient of yellow colors. The colors are arranged
from black to pure yellow.")

(cffi:defcfun ("imPaletteMagenta" %im-palette-magenta) :pointer
  "Creates a palette of a gradient of magenta colors. The colors are arranged
from black to pure magenta.")

(cffi:defcfun ("imPaletteCyan" %im-palette-cyan) :pointer
  "Creates a palette of a gradient of cyan colors. The colors are arranged
from black to pure cyan.")

(cffi:defcfun ("imPaletteRainbow" %im-palette-rainbow) :pointer
  "Creates a palette of rainbow colors. The colors are arranged in the light
wave length spectrum order (starting from purple).")

(cffi:defcfun ("imPaletteHues" %im-palette-hues) :pointer
  "Creates a palette of hues with maximum saturation.")

(cffi:defcfun ("imPaletteBlueIce" %im-palette-blue-ice) :pointer
  "Creates a palette of a gradient of blue colors. The colors are arranged
from pure blue to white.")

(cffi:defcfun ("imPaletteHotIron" %im-palette-hot-iron) :pointer
  "Creates a palette of a gradient from black to white passing trough red and
orange.")

(cffi:defcfun ("imPaletteBlackBody" %im-palette-black-body) :pointer
  "Creates a palette of a gradient from black to white passing trough red and
yellow.")

(cffi:defcfun ("imPaletteHighContrast" %im-palette-high-contrast) :pointer
  "Creates a palette with high contrast colors.")

(cffi:defcfun ("imPaletteLinear" %im-palette-linear) :pointer
  "Creates a palette of a sequence of colors from black to white with 32
linear intensity values combined with 8 hue variations.")

(cffi:defcfun ("imPaletteUniform" %im-palette-uniform) :pointer
  "Creates a palette of an uniform sub-division of colors from black to
white. This is a 2^(2.6) bits per pixel palette.")

(cffi:defcfun ("imPaletteUniformIndex" %im-palette-uniform-index) :int
  "Returns the index of the correspondent RGB color of an uniform palette."
  (color :long))

(cffi:defcfun ("imPaletteUniformIndexHalftoned" %im-palette-uniform-index-halftoned) :int
  "Returns the index of the correspondent RGB color of an uniform palette.
Uses an 8x8 ordered dither to lookup the index in a halftone matrix. The
spatial position used by the halftone method."
  (color :long)
  (x :int)
  (y :int))
