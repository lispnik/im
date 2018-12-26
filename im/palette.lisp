(in-package #:im)

(export '(palette-new
          palette-new-from
          palette-release
          palette-duplicate))

(defalias palette-new #'im-cffi::%im-palette-new
  "Allocates memory for the palette data. This ensures allocation and
release in the same module by the correct functions.")

(defun palette-new-from (sequence)
  "Allocates a palette from a SEQUENCE of integers."
  (let* ((count (length sequence))
         (palette (palette-new count)))
    (loop for i below count
          do (setf (cffi:mem-ref palette :long)
                   (elt sequence i)))))

(defalias palette-release #'im-cffi::%im-palette-release
  "Releases memory for the palette data. This ensures allocation and
release in the same module by the correct functions.")

(defalias palette-duplicate #'im-cffi::%im-palette-duplicate
  "Duplicate a palette data using PALETTE-NEW.")
