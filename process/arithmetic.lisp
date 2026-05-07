(defpackage #:im-arithmetic
  (:use #:common-lisp)
  (:export #:unary-op
           #:binary-op
           #:const-op
           #:bitwise-op
           #:bitwise-not
           #:bit-mask
           #:bit-plane
           #:binary-mask
           #:blend
           #:blend-const
           #:compose
           #:multiply-conj
           #:back-sub
           #:auto-covariance
           #:multiple-mean
           #:multiple-std-dev
           #:multiple-median
           #:split-complex
           #:merge-complex
           #:negative
           #:tone-gamut
           #:un-normalize
           #:direct-conv
           #:calc-auto-gamma
           #:posterize
           #:pixelate))

(in-package #:im-arithmetic)

;;; Unary / binary -----------------------------------------------------

(defun unary-op (src-image dst-image op)
  "Apply unary arithmetic OP (an UNARY-OP keyword like :unary-op-absolute,
:unary-op-square, :unary-op-log, etc.). Can run in-place."
  (im-process-cffi::%im-process-un-arithmetic-op src-image dst-image op))

(defun binary-op (src-image1 src-image2 dst-image op)
  "Apply binary arithmetic OP to two images (BINARY-OP keyword like
:binary-op-add, :binary-op-sub, :binary-op-mul, :binary-op-div, etc.).
Can run in-place."
  (im-process-cffi::%im-process-arithmetic-op src-image1 src-image2 dst-image op))

(defun const-op (src-image src-const dst-image op)
  "Apply binary OP between SRC-IMAGE and the scalar SRC-CONST."
  (im-process-cffi::%im-process-arithmetic-const-op
   src-image (coerce src-const 'double-float) dst-image op))

;;; Bitwise -----------------------------------------------------------

(defun bitwise-op (src-image1 src-image2 dst-image op)
  "Apply bitwise OP (:bitwise-op-and / :bitwise-op-or / :bitwise-op-xor)
between two integer-typed images."
  (im-process-cffi::%im-process-bitwise-op src-image1 src-image2 dst-image op))

(defun bitwise-not (src-image dst-image)
  "Per-pixel bitwise NOT."
  (im-process-cffi::%im-process-bitwise-not src-image dst-image))

(defun bit-mask (src-image dst-image mask op)
  "Apply OP between SRC-IMAGE and the constant byte MASK."
  (im-process-cffi::%im-process-bit-mask src-image dst-image mask op))

(defun bit-plane (src-image dst-image plane &key reset)
  "Extract or clear a single bit plane. With RESET nil (default),
DST gets 1 where the bit is set, 0 otherwise. With RESET t, DST
copies SRC with that bit cleared."
  (im-process-cffi::%im-process-bit-plane src-image dst-image plane (if reset t nil)))

(defun binary-mask (src-image dst-image mask-image)
  "Copy SRC pixels where MASK-IMAGE is non-zero; min(SRC) elsewhere."
  (im-process-cffi::%im-process-binary-mask src-image dst-image mask-image))

;;; Blend / compose ----------------------------------------------------

(defun blend (src-image1 src-image2 alpha-image dst-image)
  "Per-pixel alpha blend: dst = src1*alpha + src2*(1-alpha)."
  (im-process-cffi::%im-process-blend src-image1 src-image2 alpha-image dst-image))

(defun blend-const (src-image1 src-image2 dst-image alpha)
  "Constant alpha blend: dst = src1*alpha + src2*(1-alpha).
NB: ALPHA=1 returns SRC-IMAGE1 (alpha=0 returns SRC-IMAGE2)."
  (im-process-cffi::%im-process-blend-const
   src-image1 src-image2 dst-image (coerce alpha 'double-float)))

(defun compose (src-image1 src-image2 dst-image)
  "Compose two RGBA images using their alpha channels."
  (im-process-cffi::%im-process-compose src-image1 src-image2 dst-image))

;;; Complex arithmetic -------------------------------------------------

(defun split-complex (src-image dst1 dst2 &key polar)
  "Split a complex image into two real planes. With POLAR T, result
is magnitude/phase; otherwise real/imaginary."
  (im-process-cffi::%im-process-split-complex src-image dst1 dst2 (if polar 1 0)))

(defun merge-complex (src1 src2 dst-image &key polar)
  "Merge two real planes into a complex image."
  (im-process-cffi::%im-process-merge-complex src1 src2 dst-image (if polar 1 0)))

(defun multiply-conj (src-image1 src-image2 dst-image)
  "Per-pixel src1 * conj(src2) on complex images."
  (im-process-cffi::%im-process-multiply-conj src-image1 src-image2 dst-image))

;;; Statistical combinations ------------------------------------------

(defun multiple-mean (image-list dst-image)
  "Pixel-wise mean of a sequence of images."
  (let ((n (length image-list)))
    (cffi:with-foreign-object (lp :pointer n)
      (loop for i below n
            for img in image-list
            do (setf (cffi:mem-aref lp :pointer i) img))
      (im-process-cffi::%im-process-multiple-mean lp n dst-image))))

(defun multiple-std-dev (image-list mean-image dst-image)
  "Pixel-wise standard deviation of a sequence of images, using
MEAN-IMAGE (typically the result of MULTIPLE-MEAN)."
  (let ((n (length image-list)))
    (cffi:with-foreign-object (lp :pointer n)
      (loop for i below n
            for img in image-list
            do (setf (cffi:mem-aref lp :pointer i) img))
      (im-process-cffi::%im-process-multiple-std-dev lp n mean-image dst-image))))

(defun multiple-median (image-list dst-image)
  "Pixel-wise median of a sequence of images."
  (let ((n (length image-list)))
    (cffi:with-foreign-object (lp :pointer n)
      (loop for i below n
            for img in image-list
            do (setf (cffi:mem-aref lp :pointer i) img))
      (im-process-cffi::%im-process-multiple-median lp n dst-image))))

(defun auto-covariance (src-image mean-image dst-image)
  "Compute auto-covariance of SRC-IMAGE relative to MEAN-IMAGE."
  (im-process-cffi::%im-process-auto-covariance src-image mean-image dst-image))

(defun back-sub (src-image1 src-image2 dst-image &key (tolerance 0.0) show-diff)
  "Background subtraction: zero out pixels of SRC-IMAGE1 that are
within TOLERANCE of SRC-IMAGE2."
  (im-process-cffi::%im-process-back-sub src-image1 src-image2 dst-image
                                         (coerce tolerance 'double-float)
                                         (if show-diff 1 0)))

;;; Tone / gamma -------------------------------------------------------

(defun negative (src-image dst-image)
  "Photographic negative."
  (im-process-cffi::%im-process-negative src-image dst-image))

(defun tone-gamut (src-image dst-image op &rest params)
  "Apply a tone-gamut OP (a TONE-GAMUT-OP keyword like
:tone-gamut-normalize, :tone-gamut-pow, :tone-gamut-invert, etc.).
PARAMS are operation-specific double-floats."
  (let ((n (length params)))
    (if (zerop n)
        (im-process-cffi::%im-process-tone-gamut src-image dst-image op
                                                 (cffi:null-pointer))
        (cffi:with-foreign-object (pp :double n)
          (loop for i below n
                for v in params
                do (setf (cffi:mem-aref pp :double i) (coerce v 'double-float)))
          (im-process-cffi::%im-process-tone-gamut src-image dst-image op pp)))))

(defun un-normalize (src-image dst-image)
  "Convert a normalised float image back to its original integer range."
  (im-process-cffi::%im-process-un-normalize src-image dst-image))

(defun direct-conv (src-image dst-image)
  "Direct cast conversion (no scaling)."
  (im-process-cffi::%im-process-direct-conv src-image dst-image))

(defun calc-auto-gamma (image)
  "Estimate a sensible gamma value for IMAGE."
  (im-process-cffi::%im-process-calc-auto-gamma image))

(defun posterize (src-image dst-image level)
  "Reduce intensity levels by clearing the LEVEL least-significant
bits. Output has at most 2^(8-LEVEL) distinct byte values."
  (im-process-cffi::%im-process-posterize src-image dst-image level))

(defun pixelate (src-image dst-image box-size)
  "Pixelate by averaging non-overlapping BOX-SIZE x BOX-SIZE blocks."
  (im-process-cffi::%im-process-pixelate src-image dst-image box-size))
