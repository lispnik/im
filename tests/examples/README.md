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
  - Handles alpha channel preservation

- **canny.lua** → `canny-edge-detection` test
  - Canny edge detection with hysteresis thresholding
  - Automatic threshold estimation

- **process.lua** → `process-operations` test
  - Histogram calculation and visualization
  - RGB component splitting and merging
  - Color replacement operations
  - Bit mask operations

### Image Analysis (`analysis.lisp`)

- **analyze.lua** → `region-analysis` test
  - Region detection and labeling
  - Area measurement
  - Principal axis analysis (major/minor lengths)

- Additional analysis functions:
  - Histogram analysis tests
  - Comprehensive statistics validation

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
- Histogram visualizations
- Analysis results

## API Mapping Notes

### Lua → Common Lisp Patterns

- `im.FileImageLoad()` → `(im-file:image-load path)`
- `image:Save()` → `(im-file:image-save image path format)`
- `im.ProcessSobelConvolveNew()` → `(im-process:sobel-convolve-new image)`
- `im.CalcHistogram()` → `(im-calc:histogram image channel)`
- `im.AnalyzeFindRegions()` → `(im-analyze:find-regions binary region connectivity border)`

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