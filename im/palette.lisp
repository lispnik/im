(defpackage #:im-palette
  (:use #:common-lisp
	#:cffi
	#:serapeum)
  (:shadow #:count)
  (:export #:data
           #:count
           #:sequence
           #:new
           #:new-from
           #:release
           #:duplicate
           #:find-nearest
           #:find-color
           #:cyan
           #:uniform-index
           #:uniform-index-halftoned))

(in-package #:im-palette)

(defclass palette ()
  ((data :type cffi:foreign-pointer
         :initform (cffi:null-pointer)
         :reader data)
   (count :type integer
          :initarg :count
          :initform 256
          :reader count))
  (:documentation
   "Encapsulation of palette entries and number of palette entries."))

(defmethod initialize-instance :after ((instance palette) &key (allocate t))
  (when allocate
    (let ((count (slot-value instance 'count)))
      (setf (slot-value instance 'data)
            (im-cffi::%im-palette-new (slot-value instance 'count)))
      (loop for i below count
            do (setf (cffi:mem-aref (data instance) :long i) 0)))))

(defun sequence (palette)
  "Return the palette as a sequence of colors."
  (loop with count = (count palette)
        with data = (data palette)
        with entries
          = (make-array count
                        :element-type '(signed-byte #.(* 8 (cffi:foreign-type-size :long))))
        for i below count
        do (setf (aref entries i)
                 (cffi:mem-aref data :long i))
        finally (return entries)))

(defun  new (count)
  "Allocates memory for the palette data. This ensures allocation and
release in the same module by the correct functions. NOTE: Unlike the
C API, this function will initialize palette entries to 0."
  (make-instance 'palette :count count))

(defun new-from-sequence (sequence)
  "Allocates a palette from a SEQUENCE of integers."
  (let* ((count (length sequence))
         (palette (new count)))
    (loop for i below count
          do (setf (cffi:mem-aref (data palette) :long i)
                   (elt sequence i))
          finally (return palette))))

(defun release (palette)
  "Releases memory for the palette data. This ensures allocation and
release in the same module by the correct functions."
  (setf (slot-value palette 'data) (cffi:null-pointer))
  (im-cffi::%im-palette-release (data palette)))

(defun duplicate (palette)
  "Duplicate a palette data using NEW."
  (let* ((count (count palette))
         (new-palette (make-instance 'palette :count count)))
    (loop for i below count
          do (setf (cffi:mem-aref (data new-palette) :long i)
                   (cffi:mem-aref (data palette) :long i)))))

(defun find-nearest (palette color)
  "Searches for the nearest color on the table and returns the color
index if successful. It looks in all palette entries and finds the
minimum euclidian square distance. If the color matches the given
color it returns immediately."
  (im-cffi::%im-palette-find-nearest 
   (data palette)
   (count palette)
   color))

(defun find-color (palette color tolerance)
  "Searches for COLOR in the table and returns the color index if
successful. If TOLERANCE is 0 search for the exact match in the
palette else search for the first color that fits in the tolerance
range."
  (im-cffi::%im-palette-find-color
   (data palette)
   (count palette)
   color
   tolerance))

(defmacro %define-palette (name documentation)
  (let* ((cffi-package (find-package "IM-CFFI"))
         (cffi-function (intern (format nil "%IM-PALETTE-~A" (symbol-name name))
                                cffi-package)))
    `(progn
       (defun ,name ()
         ,documentation
         (let ((palette (make-instance 'palette :allocate nil :count 256)))
           (setf (slot-value palette 'data) (,cffi-function))
           palette))
       (export ',name))))

(%define-palette gray
  "Creates a palette of gray scale values.
The colors are arranged from black to white.")

(%define-palette red
  "Creates a palette of a gradient of red colors. 
The colors are arranged from black to pure red.")

(%define-palette green
  "Creates a palette of a gradient of green colors. 
The colors are arranged from black to pure red.")

(%define-palette blue
  "Creates a palette of a gradient of red colors. 
The colors are arranged from black to pure red.")

(%define-palette magenta
  "Creates a palette of a gradient of magenta colors. 
The colors are arranged from black to pure magenta.")

(%define-palette cian
  "Creates a palette of a gradient of cian colors. 
The colors are arranged from black to pure cian.")

(setf (symbol-function 'cyan) #'cian)

(%define-palette rainbow
  "Creates a palette of rainbow colors. 
The colors are arranged in the light wave length spectrum
order (starting from purple).")

(%define-palette hues
  "Creates a palette of hues with maximum saturation.")

(%define-palette blue-ice
  "Creates a palette of a gradient of blue colors.
The colors are arranged from pure blue to white.")

(%define-palette hot-iron
  "Creates a palette of a gradient from black to white passing through
red and orange.")

(%define-palette high-contrast
  "Creates a palette with high contrast colors.")

(%define-palette linear
  "Creates a palette of a sequence of colors from black to white with
32 linear intensity values combined with 8 hue variations.")

(%define-palette uniform
  "Creates a palette of an uniform sub-division of colors from black
to white. This is a 2^(2.6) bits per pixel palette.")

(defun uniform-index (color)
  "Returns the index of the correspondent RGB color of an uniform
palette."
  (im-cffi::%im-palette-uniform-index color))

(defun uniform-index-halftoned (color x y)
  "Returns the index of the correspondent RGB color of an uniform
palette. Uses an 8x8 ordered dither to lookup the index in a halftone
matrix. The spatial position used by the halftone method."
  (im-cffi::%im-palette-uniform-index-halftoned color x y))
