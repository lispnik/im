(in-package #:im)

(export '(palette-data
          palette-count
          palette-new
          palette-new-from
          palette-release
          palette-duplicate))

;;; Class PALETTE is a simple encapsulation of palette entries and
;;; number of palette entries.

(defclass palette ()
  ((data :initform (cffi:null-pointer)
         :reader palette-data)
   (count :type integer
          :initarg :count
          :initform 256
          :reader palette-count)))

(defmethod initialize-instance :after ((instance palette) &key)
  (let ((count (slot-value instance 'count)))
    (setf (slot-value instance 'data)
          (im-cffi::%im-palette-new (slot-value instance 'count)))
    (loop for i below count
          do (setf (cffi:mem-aref (palette-data instance) :long i) 0))))

(defun  palette-new (count)
  "Allocates memory for the palette data. This ensures allocation and
release in the same module by the correct functions. NOTE: Unlike the
C API, this function will initialize palette entries to 0."
  (make-instance 'palette :count count))

(defun palette-new-from (sequence)
  "Allocates a palette from a SEQUENCE of integers."
  (let* ((count (length sequence))
         (palette (palette-new count)))
    (loop for i below count
          do (setf (cffi:mem-aref (palette-data palette) :long i)
                   (elt sequence i)))))

(defun palette-release (palette)
  "Releases memory for the palette data. This ensures allocation and
release in the same module by the correct functions."
  (setf (slot-value palette 'data) (cffi:null-pointer))
  (im-cffi::%im-palette-release (palette-data palette)))

(defun palette-duplicate (palette)
  "Duplicate a palette data using PALETTE-NEW."
  (let* ((count (palette-count palette))
         (new-palette (make-instance 'palette :count count)))
    (loop for i below count
          do (setf (cffi:mem-aref (palette-data new-palette) :long i)
                   (cffi:mem-aref (palette-data palette) :long i)))))

(defun palette-find-nearest (palette color)
  (im-cffi::%im-palette-find-nearest 
   (palette-data palette)
   (palette-count palette)
   color))
