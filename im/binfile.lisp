(defpackage #:im-binfile
  (:use #:common-lisp)
  (:export #:open-file
           #:new-file
           #:close-file
           #:error-status
           #:size
           #:byte-order
           #:cpu-byte-order
           #:read-bytes
           #:write-bytes
           #:read-line-from
           #:skip-line
           #:read-integer
           #:read-real
           #:seek-to
           #:seek-offset
           #:seek-from
           #:tell
           #:end-of-file-p
           #:set-current-module
           #:memory-release
           #:with-bin-file
           #:with-current-module
           #:make-memory-file-name
           #:+rawfile+
           #:+stream+
           #:+memfile+
           #:+subfile+
           #:+filehandle+))

(in-package #:im-binfile)

;;; Module constants ---------------------------------------------------

(defparameter +rawfile+    :bin-file-module-rawfile)
(defparameter +stream+     :bin-file-module-stream)
(defparameter +memfile+    :bin-file-module-memfile)
(defparameter +subfile+    :bin-file-module-subfile)
(defparameter +filehandle+ :bin-file-module-filehandle)

;;; Lifecycle ----------------------------------------------------------

(defun open-file (filename)
  "Open FILENAME for reading using the current binfile module.
Returns a foreign pointer. Returns NIL if the open failed.
Pair with CLOSE-FILE."
  (let ((p (im-cffi::%im-bin-file-open filename)))
    (if (cffi:null-pointer-p p) nil p)))

(defun new-file (filename)
  "Create FILENAME for writing using the current binfile module.
Returns a foreign pointer (or NIL). Pair with CLOSE-FILE."
  (let ((p (im-cffi::%im-bin-file-new filename)))
    (if (cffi:null-pointer-p p) nil p)))

(defun close-file (bin-file)
  "Close BIN-FILE."
  (im-cffi::%im-bin-file-close bin-file))

(defun error-status (bin-file)
  "Return non-zero if the most recent operation on BIN-FILE failed."
  (im-cffi::%im-bin-file-error bin-file))

(defun size (bin-file)
  "Return the size of BIN-FILE in bytes."
  (im-cffi::%im-bin-file-size bin-file))

;;; Byte order ---------------------------------------------------------

(defun byte-order (bin-file &optional new-order)
  "Get or set the file's byte order. NEW-ORDER, if supplied, is
:little or :big and the previous order is returned. Without an
argument the current order is returned."
  (let ((value (cond ((null new-order) -1)
                     ((eq new-order :little) im-cffi::*little-endian*)
                     ((eq new-order :big)    im-cffi::*big-endian*)
                     (t (error "byte-order argument must be :little or :big")))))
    (let ((prev (im-cffi::%im-bin-file-byte-order bin-file value)))
      (if (zerop prev) :little :big))))

(defun cpu-byte-order ()
  "Return :little or :big for the host CPU."
  (cffi:with-foreign-object (probe :uint16)
    (setf (cffi:mem-ref probe :uint16) #x0102)
    (if (= (cffi:mem-aref probe :uint8 0) #x02) :little :big)))

;;; Read / write -------------------------------------------------------

(defun read-bytes (bin-file buffer count size-of)
  "Read COUNT values of SIZE-OF bytes each into BUFFER (a foreign
pointer), with byte-order conversion as configured. Returns the
number of values actually read."
  (im-cffi::%im-bin-file-read bin-file buffer count size-of))

(defun write-bytes (bin-file buffer count size-of)
  "Write COUNT values of SIZE-OF bytes each from BUFFER. Note: the C
API may swap bytes in place if byte orders differ, so BUFFER may be
modified after this call. Returns the number of values written."
  (im-cffi::%im-bin-file-write bin-file buffer count size-of))

;;; Text helpers ------------------------------------------------------

(defun read-line-from (bin-file &optional (max-size 4096))
  "Read up to MAX-SIZE bytes from BIN-FILE, terminated by a newline.
Returns the line as a string (without the newline). Returns NIL on
error or eof."
  (cffi:with-foreign-objects ((buffer :char max-size)
                              (size-ptr :int))
    (setf (cffi:mem-ref size-ptr :int) max-size)
    (let ((ok (im-cffi::%im-bin-file-read-line bin-file buffer size-ptr)))
      (when (= ok 1)
        (cffi:foreign-string-to-lisp
         buffer :count (cffi:mem-ref size-ptr :int))))))

(defun skip-line (bin-file)
  "Advance past the next line break."
  (im-cffi::%im-bin-file-skip-line bin-file))

(defun read-integer (bin-file)
  "Parse and return an integer from the current position. Returns
NIL on parse failure."
  (cffi:with-foreign-object (val :int)
    (when (= 1 (im-cffi::%im-bin-file-read-integer bin-file val))
      (cffi:mem-ref val :int))))

(defun read-real (bin-file)
  "Parse and return a real number from the current position. Returns
NIL on parse failure."
  (cffi:with-foreign-object (val :double)
    (when (= 1 (im-cffi::%im-bin-file-read-real bin-file val))
      (cffi:mem-ref val :double))))

;;; Seeking -----------------------------------------------------------

(defun seek-to (bin-file offset)
  "Seek to OFFSET from the start of the file."
  (im-cffi::%im-bin-file-seek-to bin-file offset))

(defun seek-offset (bin-file delta)
  "Seek by DELTA bytes from the current position. DELTA may be
negative."
  (im-cffi::%im-bin-file-seek-offset bin-file delta))

(defun seek-from (bin-file delta)
  "Seek by DELTA bytes from the end of the file (DELTA usually
negative)."
  (im-cffi::%im-bin-file-seek-from bin-file delta))

(defun tell (bin-file)
  "Return the current position."
  (im-cffi::%im-bin-file-tell bin-file))

(defun end-of-file-p (bin-file)
  "Return T if BIN-FILE is at end of file."
  (not (zerop (im-cffi::%im-bin-file-end-of-file bin-file))))

;;; Module switching --------------------------------------------------

(defun set-current-module (module)
  "Set the active binfile module. MODULE is one of :bin-file-module-rawfile,
:bin-file-module-stream, :bin-file-module-memfile,
:bin-file-module-subfile, :bin-file-module-filehandle. Returns the
previous module as an integer."
  (im-cffi::%im-bin-file-set-current-module module))

(defun memory-release (buffer)
  "Free a memory buffer that was allocated by IM during a memfile
write. Pass the BUFFER pointer returned via the
imBinMemoryFileName struct."
  (im-cffi::%im-bin-memory-release buffer))

;;; Convenience macros ------------------------------------------------

(defmacro with-bin-file ((var open-form) &body body)
  "Bind VAR to the result of OPEN-FORM (e.g. (open-file ...)) and
ensure CLOSE-FILE is invoked, even on non-local exit."
  `(let ((,var ,open-form))
     (unless ,var (error "BinFile open failed"))
     (unwind-protect (progn ,@body)
       (close-file ,var))))

(defmacro with-current-module ((module) &body body)
  "Set the binfile module to MODULE for the dynamic extent of BODY,
restoring the previous module on exit."
  (let ((prev (gensym "PREV")))
    `(let ((,prev (set-current-module ,module)))
       (unwind-protect (progn ,@body)
         (im-cffi::%im-bin-file-set-current-module ,prev)))))

(defun make-memory-file-name (buffer size &optional (reallocate 0.0))
  "Allocate and populate an imBinMemoryFileName foreign struct
suitable for use as the FILENAME argument to OPEN-FILE / NEW-FILE
when the current module is :bin-file-module-memfile. The caller
must FOREIGN-FREE the returned pointer when done.

BUFFER is a foreign pointer to the memory buffer. SIZE is its size
in bytes. REALLOCATE is the growth factor for writes (0.0 disables
growth)."
  (let ((p (cffi:foreign-alloc '(:struct im-cffi::im-bin-memory-file-name))))
    (setf (cffi:foreign-slot-value p '(:struct im-cffi::im-bin-memory-file-name)
                                    'im-cffi::buffer)
          buffer
          (cffi:foreign-slot-value p '(:struct im-cffi::im-bin-memory-file-name)
                                    'im-cffi::size)
          size
          (cffi:foreign-slot-value p '(:struct im-cffi::im-bin-memory-file-name)
                                    'im-cffi::reallocate)
          (coerce reallocate 'single-float))
    p))
