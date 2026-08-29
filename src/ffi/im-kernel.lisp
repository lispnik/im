;;;; src/ffi/im-kernel.lisp — DRAFTED by tools/gen-bindings.lisp.
;;;;
;;;; Source: im_kernel.h
;;;; Hand corrections below this line are expected and are kept;
;;;; re-run the generator into a clean tree and diff.

(in-package #:im.ffi)

(cffi:defcfun ("imKernelSobel" %im-kernel-sobel) im-image
  "Creates a kernel with the following values:")

(cffi:defcfun ("imKernelPrewitt" %im-kernel-prewitt) im-image
  "Creates a kernel with the following values:")

(cffi:defcfun ("imKernelKirsh" %im-kernel-kirsh) im-image
  "Creates a kernel with the following values:")

(cffi:defcfun ("imKernelLaplacian4" %im-kernel-laplacian4) im-image
  "Creates a kernel with the following values:")

(cffi:defcfun ("imKernelLaplacian8" %im-kernel-laplacian8) im-image
  "Creates a kernel with the following values:")

(cffi:defcfun ("imKernelLaplacian5x5" %im-kernel-laplacian5x5) im-image
  "Creates a kernel with the following values:")

(cffi:defcfun ("imKernelLaplacian7x7" %im-kernel-laplacian7x7) im-image
  "Creates a kernel with the following values:")

(cffi:defcfun ("imKernelGradian3x3" %im-kernel-gradian3x3) im-image
  "Creates a kernel with the following values: A pixel minus the one below
it. Note that this measures the vertical difference where
imKernelGradian7x7() measures the horizontal one, so the two are not one
operator at two sizes despite the shared name.")

(cffi:defcfun ("imKernelGradian7x7" %im-kernel-gradian7x7) im-image
  "Creates a kernel with the following values:")

(cffi:defcfun ("imKernelSculpt" %im-kernel-sculpt) im-image
  "Creates a kernel with the following values:")

(cffi:defcfun ("imKernelMean3x3" %im-kernel-mean3x3) im-image
  "Creates a kernel with the following values:")

(cffi:defcfun ("imKernelMean5x5" %im-kernel-mean5x5) im-image
  "Creates a kernel with the following values:")

(cffi:defcfun ("imKernelCircularMean5x5" %im-kernel-circular-mean5x5) im-image
  "Creates a kernel with the following values:")

(cffi:defcfun ("imKernelMean7x7" %im-kernel-mean7x7) im-image
  "Creates a kernel with the following values:")

(cffi:defcfun ("imKernelCircularMean7x7" %im-kernel-circular-mean7x7) im-image
  "Creates a kernel with the following values:")

(cffi:defcfun ("imKernelGaussian3x3" %im-kernel-gaussian3x3) im-image
  "Creates a kernel with the following values:")

(cffi:defcfun ("imKernelGaussian5x5" %im-kernel-gaussian5x5) im-image
  "Creates a kernel with the following values:")

(cffi:defcfun ("imKernelBarlett5x5" %im-kernel-barlett5x5) im-image
  "Creates a kernel with the following values:")

(cffi:defcfun ("imKernelTopHat5x5" %im-kernel-top-hat5x5) im-image
  "Creates a kernel with the following values:")

(cffi:defcfun ("imKernelTopHat7x7" %im-kernel-top-hat7x7) im-image
  "Creates a kernel with the following values:")

(cffi:defcfun ("imKernelEnhance" %im-kernel-enhance) im-image
  "Creates a kernel with the following values:")
