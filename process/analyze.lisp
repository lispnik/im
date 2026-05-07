(defpackage #:im-analyze
  (:use #:common-lisp)
  (:export #:find-regions
           #:measure-area
           #:measure-perimeter
           #:measure-perim-area
           #:measure-centroid
           #:measure-principal-axis
           #:measure-holes
           #:perimeter-line
           #:fill-holes
           #:remove-by-area))

(in-package #:im-analyze)

(defun find-regions (src-image dst-image &key (connect 4) touch-border)
  "Label connected foreground regions of SRC-IMAGE into DST-IMAGE
(IM_GRAY/IM_USHORT). CONNECT is 4 or 8 for connectivity. With
TOUCH-BORDER T, regions touching the image border are kept.
Returns the number of distinct regions found."
  (cffi:with-foreign-object (rc :int)
    (im-process-cffi::%im-analyze-find-regions
     src-image dst-image connect (if touch-border t nil) rc)
    (cffi:mem-ref rc :int)))

(defun measure-area (label-image region-count)
  "Return a vector of pixel areas, one per region. LABEL-IMAGE must be
the result of FIND-REGIONS."
  (cffi:with-foreign-object (ap :int region-count)
    (im-process-cffi::%im-analyze-measure-area label-image ap region-count)
    (loop with v = (make-array region-count :element-type '(signed-byte 32))
          for i below region-count
          do (setf (aref v i) (cffi:mem-aref ap :int i))
          finally (return v))))

(defun measure-perimeter (label-image region-count)
  "Return a vector of perimeter lengths, one per region."
  (cffi:with-foreign-object (pp :double region-count)
    (im-process-cffi::%im-analyze-measure-perimeter label-image pp region-count)
    (loop with v = (make-array region-count :element-type 'double-float)
          for i below region-count
          do (setf (aref v i) (cffi:mem-aref pp :double i))
          finally (return v))))

(defun measure-perim-area (label-image region-count)
  "Return a vector of perimeter/area ratios (a fragmentation index),
one per region."
  (cffi:with-foreign-object (pp :double region-count)
    (im-process-cffi::%im-analyze-measure-perim-area label-image pp region-count)
    (loop with v = (make-array region-count :element-type 'double-float)
          for i below region-count
          do (setf (aref v i) (cffi:mem-aref pp :double i))
          finally (return v))))

(defun measure-centroid (label-image area-vector region-count)
  "Return two vectors (CX, CY) of centroid coordinates, one per region.
AREA-VECTOR must be the result of MEASURE-AREA on the same label image."
  (cffi:with-foreign-objects ((ap :int region-count)
                              (cx :double region-count)
                              (cy :double region-count))
    ;; copy AREA-VECTOR into the foreign buffer
    (loop for i below region-count
          do (setf (cffi:mem-aref ap :int i) (aref area-vector i)))
    (im-process-cffi::%im-analyze-measure-centroid
     label-image ap region-count cx cy)
    (let ((cx-out (make-array region-count :element-type 'double-float))
          (cy-out (make-array region-count :element-type 'double-float)))
      (loop for i below region-count
            do (setf (aref cx-out i) (cffi:mem-aref cx :double i)
                     (aref cy-out i) (cffi:mem-aref cy :double i)))
      (values cx-out cy-out))))

(defun measure-principal-axis (label-image area-vector centroid-x centroid-y region-count)
  "Return four vectors of principal-axis measurements: (major-slope,
major-length, minor-slope, minor-length)."
  (cffi:with-foreign-objects ((ap :int region-count)
                              (cx :double region-count)
                              (cy :double region-count)
                              (mjs :double region-count)
                              (mjl :double region-count)
                              (mns :double region-count)
                              (mnl :double region-count))
    (loop for i below region-count do
      (setf (cffi:mem-aref ap :int i) (aref area-vector i)
            (cffi:mem-aref cx :double i) (aref centroid-x i)
            (cffi:mem-aref cy :double i) (aref centroid-y i)))
    (im-process-cffi::%im-analyze-measure-principal-axis
     label-image ap cx cy region-count mjs mjl mns mnl)
    (let ((out-mjs (make-array region-count :element-type 'double-float))
          (out-mjl (make-array region-count :element-type 'double-float))
          (out-mns (make-array region-count :element-type 'double-float))
          (out-mnl (make-array region-count :element-type 'double-float)))
      (loop for i below region-count do
        (setf (aref out-mjs i) (cffi:mem-aref mjs :double i)
              (aref out-mjl i) (cffi:mem-aref mjl :double i)
              (aref out-mns i) (cffi:mem-aref mns :double i)
              (aref out-mnl i) (cffi:mem-aref mnl :double i)))
      (values out-mjs out-mjl out-mns out-mnl))))

(defun measure-holes (label-image region-count &key (connect 4))
  "Return three vectors per region: (HOLES-COUNT HOLES-AREA HOLES-PERIM)."
  (cffi:with-foreign-objects ((hc :int region-count)
                              (ha :int region-count)
                              (hp :double region-count))
    (im-process-cffi::%im-analyze-measure-holes
     label-image connect region-count hc ha hp)
    (let ((out-hc (make-array region-count :element-type '(signed-byte 32)))
          (out-ha (make-array region-count :element-type '(signed-byte 32)))
          (out-hp (make-array region-count :element-type 'double-float)))
      (loop for i below region-count do
        (setf (aref out-hc i) (cffi:mem-aref hc :int i)
              (aref out-ha i) (cffi:mem-aref ha :int i)
              (aref out-hp i) (cffi:mem-aref hp :double i)))
      (values out-hc out-ha out-hp))))

(defun perimeter-line (src-image dst-image)
  "Extract the boundary pixels of SRC-IMAGE into DST-IMAGE."
  (im-process-cffi::%im-process-perimeter-line src-image dst-image))

(defun fill-holes (src-image dst-image &optional (connect 4))
  "Fill background components fully enclosed by foreground."
  (im-process-cffi::%im-process-fill-holes src-image dst-image connect))

(defun remove-by-area (src-image dst-image start-size end-size &key (connect 4) inside)
  "Remove regions whose area falls outside [START-SIZE, END-SIZE].
With INSIDE T, removes regions inside that range instead."
  (im-process-cffi::%im-process-remove-by-area
   src-image dst-image connect start-size end-size (if inside t nil)))
