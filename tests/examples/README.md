# IM Examples Test Suite

This directory contains Common Lisp ports of the Lua examples from the IM library (`tecgraf/im/html/examples/`). These examples demonstrate various IM library capabilities and serve as both documentation and integration tests for the Common Lisp IM bindings.

## Overview

The examples are organized into test suites using FiveAM:

- **info-examples**: File information and metadata extraction
- **processing-examples**: Image processing operations (filters, edge detection, etc.)
- **analysis-examples**: Image analysis and measurements

## Test Images

The following test images are used by the examples:

- `lena.jpg` - Classic test image for processing demonstrations
- `flower.gif` - GIF format test image
- `flower.jpg` - JPEG version of flower image
- `rice.png` - Grayscale image used for region analysis

## Ported Examples

### File Information (`info.lisp`)

- **info.lua** → `print-image-info` function + tests
  - Displays comprehensive file and image metadata
  - Shows format, compression, dimensions, color space info
  - Lists image attributes and their values

- **stats.lua** → `print-image-stats` function + tests
  - Calculates and displays image statistics (min, max, mean)
  - Handles both grayscale and multi-channel images

### Image Processing (`processing.lisp`)

- **sobel.lua** → `sobel-edge-detection` test
  - Applies Sobel edge detection filter

- **canny.lua** → `canny-edge-detection` test
  - Canny edge detection with hysteresis thresholding
  - Automatic threshold estimation

- **process.lua** → `split-and-merge-components`, `replace-color-operation`
  and `bit-mask-operation` tests
  - RGB component splitting and merging (verified to round-trip losslessly)
  - Color replacement operations
  - Bit mask operations

### Image Analysis (`analysis.lisp`)

- **analyze.lua** → `region-analysis` test
  - Region detection and labeling
  - Area and centroid measurement
  - Principal axis analysis (major/minor lengths)

- Additional analysis tests:
  - `region-holes-and-perimeter` — perimeter and hole measurements
  - `color-count-analysis` — distinct colour counting
  - `statistics-analysis` — comprehensive statistics validation

### Not Ported

- The histogram section of **process.lua** (histogram calculation and its
  GIF visualization) and the histogram assertions that used to sit in
  `analysis.lisp`. `im-calc` has no `histogram`/`gray-histogram` binding
  yet — see the `;;; TODO` in `process/statistics.lisp` — and `im-render`
  does not export the `render-op` entry point the Lua original draws
  with. `im-calc:count-colors` covers the nearest available ground.

## Running the Examples

### As Part of Full Test Suite

```lisp
(asdf:test-system :im-tests)
```

### Examples Only

```lisp
(asdf:load-system :im-tests)
(im-tests:run-examples-suite)
```

### Standalone Script

```bash
# Make sure IM libraries are installed and accessible
./run-examples.lisp
```

Or:

```bash
sbcl --script run-examples.lisp
```

## Dependencies

- IM native libraries (im, im_process)
- Common Lisp IM bindings (:im, :im-process packages)
- FiveAM testing framework

## Test Output

The examples generate temporary output files in the test directory for verification. These include:

- Processed images (edge detection results, filtered images)
- Component images (R, G, B channels)
- Merged and colour-replaced images

## API Mapping Notes

### Lua → Common Lisp Patterns

- `im.FileImageLoad()` → `(im-file:image-load path)`
- `image:Save()` → `(im-file:image-save path format image)` — note the
  path comes first, and the image last
- `im.ProcessSobelConvolveNew()` → `(im-convolve:sobel src dst)`
- `im.ProcessCannyNew()` → `(im-convolve:canny src dst &optional stddev)`
- `im.ProcessHysteresisThresEstimate()` → `(im-threshold:hysteresis-estimate image)`
- `im.ProcessPercentThreshold()` → `(im-threshold:percent src dst percent)`
- `im.ConvertColorSpace()` → `(im-convert:to-color-space src dst)`
- `im.ProcessSplitComponents()` → `(im-color:split-components src &rest dsts)`
- `im.ProcessMergeComponents()` → `(im-color:merge-components list-of-src dst)`
- `im.ProcessBitMask()` → `(im-arithmetic:bit-mask src dst mask op)`
- `im.AnalyzeFindRegions()` → `(im-analyze:find-regions binary region :connect 4 :touch-border nil)`
- `im.CalcImageStatistics()` → `(im-calc:image-statistics image)`, returning
  a vector of `stats` objects read with `im-calc:stats-min` / `-max` / `-mean`

Most of the `im-process` operations live in focused packages
(`im-convolve`, `im-threshold`, `im-color`, `im-arithmetic`, `im-analyze`,
`im-calc`, `im-convert`) rather than in `im-process` itself, which only
exports the `counter-aborted` condition.

Measurement functions (`measure-area`, `measure-perimeter`,
`measure-principal-axis`, …) and file attribute values return **vectors**,
so index them with `aref`/`elt` rather than `nth`.

### Memory Management

The Common Lisp bindings use the `with-image` macro for automatic memory management:

```lisp
(with-image (img (im-file:image-load "test.jpg"))
  ;; Use img here
  ;; Automatically destroyed when exiting scope
  )
```

### Error Handling

The CL bindings use Common Lisp conditions instead of Lua error codes:

```lisp
(handler-case
    (im-file:image-load "nonexistent.jpg")
  (error (e)
    (format t "Error loading image: ~A~%" e)))
```

## Future Enhancements

Additional Lua examples that could be ported:

- Video processing examples (`animate_gif.lua`, `makevideo.lua`)
- OpenGL rendering examples (`tesselation_opengl.lua`)
- Capture examples (`capture.lua`, `screencapture.lua`)
- More processing examples (`fft.lua`, `hough_lines.lua`)

## Contributing

When adding new example ports:

1. Add the `.lisp` file to the `tests/examples/` directory
2. Update the `:components` list in `im-tests.asd`
3. Follow the existing test patterns with FiveAM `def-suite*` and `test` forms
4. Use `with-image` for memory management
5. Include verification assertions in tests
6. Document any API differences in this README