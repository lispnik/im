(defpackage #:im-image
  (:use #:common-lisp
	#:cffi
	#:serapeum)
  (:shadow #:reshape)
  (:export #:create
           #:init
           #:create-based
           #:destroy
           #:add-alpha
           #:alpha
           #:remove-alpha
           #:reshape
           #:copy
           #:copy-data
           #:copy-attributes
           #:merge-attributes
           #:copy-plane
           #:duplicate
           #:clone
           #:clear
           #:bitmap-p
           #:palette
           #:match-size-p
           #:match-color-p
           #:match-data-type-p
           #:match-color-space-p
           #:match-p
           #:set-map
           #:set-binary
           #:set-gray
           #:make-binary
           #:make-gray
           #:width
           #:height
	   #:size
	   #:data))

(in-package #:im-image)

(defalias create #'im-cffi::%im-image-create
  "Creates a new image.")

(defun init
    (width height color-mode-config color-space data-type data-ptr palette)
  "Initializes the image structure but do not allocate image data. The
only addtional flag that color-mode can have here is
:COLOR-MODE-CONFIG-ALPHA."
  (im-cffi::%im-image-init
   width height
   (im::%encode-color-mode color-mode-config color-space)
   data-type
   data-ptr
   (im-palette:data palette)
   (im-palette:count palette)))

(defun create-based (im-image &optional width height color-space data-type)
  "Creates a new image based on an existing one. The image atributes
always are copied. HasAlpha is copied."
  (im-cffi::%im-image-create-based
   im-image
   (or width -1)
   (or height -1)
   (if color-space
       (cffi:foreign-enum-value 'im-cffi::color-space color-space)
       -1)
   (if data-type
       (cffi:foreign-enum-value 'im-cffi::data-type data-type)
       -1)))

(defun destroy (im-image &optional (destroy-data-p t))
  "Destroys the image and frees the memory used. If DESTROY-DATA-P is
non-NIL then also destroy the image data."
  (unless destroy-data-p
    (setf (cffi:foreign-slot-value
           im-image
           '(:struct im-cffi::im-image-struct) 'im-cffi::data)
          (cffi:null-pointer)))
  (im-cffi::%im-image-destroy im-image))

(defalias add-alpha #'im-cffi::%im-image-add-alpha
  "Adds an alpha channel plane and sets its value to
0 (transparent).")

(defun (setf alpha) (new-alpha im-image)
  "Sets the alpha channel plane to a constant."
  (im-cffi::%im-image-set-alpha im-image (coerce new-alpha 'single-float))  
  new-alpha)

(defalias remove-alpha #'im-cffi::%im-image-add-alpha
  "Removes the alpha channel plane if any.")

(defalias reshape #'im-cffi::%im-image-reshape
  "Changes the buffer size. Reallocate internal buffers if the new
size is larger than the original.")

(defalias copy #'im-cffi::%im-image-copy
  "Copy image data and attributes from one image to another. Images
must have the same size and type.")

(defalias copy-data #'im-cffi::%im-image-copy-data
  "Copy image data only fom one image to another. Images must have the
same size and type.")

(defalias copy-attributes #'im-cffi::%im-image-copy-attributes
  "Copies the image attributes from SRC to DST. Includes the pallete
if defined in both images.")

(defalias merge-attributes #'im-cffi::%im-image-merge-attributes
  "Merges the image attributes from SRC to DST. Attributes that exist
in DST are not replaced. Does NOT include the palette.")

(defalias copy-plane #'im-cffi::%im-image-copy-plane
  "Copy one image plane fom one image to another. Images must have the
same size and type.")

(defalias duplicate #'im-cffi::%im-image-duplicate
  "Creates a copy of the image.")

(defalias clone #'im-cffi::%im-image-clone
  "Creates a clone of the image. i.e. same attributes but ignore
contents.")

;;; TODO set attribute etc.

(defalias clear #'im-cffi::%im-image-clear
  "Sets all image data to zero. But if color space is YCBCR, LAB or
LUV, and data type is BYTE or USHORT, then data is initialized with
128 or 32768 accordingly. Alpha is initialized as transparent (0).")

(defalias bitmap-p #'im-cffi::%im-image-is-bitmap
  "Indicates that the image can be viewed in common graphic
devices. Data type must be :DATA-TYPE-BYTE. Color space can
be :COLOR-SPACE-RGB, :COLOR-SPACE-MAP, :COLOR-SPACE-GRAY
or :COLOR-SPACE-BINARY.")

(defun (setf palette) (new-palette im-image)
  "Changes the image palette. This will destroy the existing palette
and replace it with the given palette pointer. Only the pointer is
stored, so the palette should be a new palette and it can not be a
static array."
  (im-cffi::%im-image-set-palette
   im-image
   (im-palette:data new-palette)
   (im-palette:count new-palette))
  new-palette)

(defalias match-size-p #'im-cffi::%im-image-match-size
  "Returns non-NIL if the images match width and height.")

(defalias match-color-p #'im-cffi::%im-image-match-color
  "Returns T if the images match color mode and data type.")

(defalias match-data-type-p #'im-cffi::%im-image-match-data-type
  "Returns T if the images match width, height and data type.")

(defalias match-color-space-p #'im-cffi::%im-image-match-color-space
  "Returns T if the images match width, height and color space.")

(defalias match-p #'im-cffi::%im-image-match
  "Returns T if the images match in width, height, data type and color
space.")

(defalias set-map #'im-cffi::%im-image-set-map
  "Changes the image color space to map by just changing the color
space. Image must be BINARY or GRAY/BYTE.")

(defalias set-binary #'im-cffi::%im-image-set-binary
  "Changes the image color space to binary by just changing the color
space and the palette. Image must be MAP or GRAY/BYTE.")

(defalias set-gray #'im-cffi::%im-image-set-gray
  "Changes the image color space to gray by just changing the color
space and the palette. Image must be BINARY or MAP. Palette is changed
only if image was BINARY.")

(defalias make-binary #'im-cffi::%im-image-make-binary
  "Changes a gray BYTE data (0,255) into a binary data (0,1), done
in-place. Color space is not changed. Data type must be IM_BYTE.")

(defalias make-gray #'im-cffi::%im-image-make-gray
  "Changes a binary data (0,1) into a gray BYTE data (0,255), done
in-place. Color space is not changed. Data type must be IM_BYTE.")

(defun width (im-image)
  (cffi:foreign-slot-value
   im-image '(:struct im-cffi::im-image-struct) 'im-cffi::width))

(defun height (im-image)
  (cffi:foreign-slot-value
   im-image '(:struct im-cffi::im-image-struct) 'im-cffi::height))

(defun size (im-image)
  (cffi:foreign-slot-value
   im-image '(:struct im-cffi::im-image-struct) 'im-cffi::size))

(defun data (im-image plane)
  ;; FIXME do better here with bounds for plane
  (cffi:mem-aref
   (cffi:foreign-slot-value
    im-image '(:struct im-cffi::im-image-struct) 'im-cffi::data)
   :pointer
   plane))
