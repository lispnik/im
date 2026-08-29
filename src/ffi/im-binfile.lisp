;;;; src/ffi/im-binfile.lisp — DRAFTED by tools/gen-bindings.lisp.
;;;;
;;;; Source: im_binfile.h
;;;; Hand corrections below this line are expected and are kept;
;;;; re-run the generator into a clean tree and diff.

(in-package #:im.ffi)

;;; imBinFileModule
(cffi:defcenum bin-file-module
  :bin-file-module-rawfile
  :bin-file-module-stream
  :bin-file-module-memfile
  :bin-file-module-subfile
  :bin-file-module-filehandle
  :bin-file-module-iocustom0)

(cffi:defcfun ("imBinFileOpen" %im-bin-file-open) im-bin-file
  "Opens an existant binary file for reading. The default file byte order is
the CPU byte order. Returns NULL if failed."
  (p-file-name :string))

(cffi:defcfun ("imBinFileNew" %im-bin-file-new) im-bin-file
  "Creates a new binary file for writing. The default file byte order is the
CPU byte order. Returns NULL if failed."
  (p-file-name :string))

(cffi:defcfun ("imBinFileClose" %im-bin-file-close) :void
  "Closes the file."
  (bfile im-bin-file))

(cffi:defcfun ("imBinFileError" %im-bin-file-error) :int
  "Indicates that was an error on the last operation."
  (bfile im-bin-file))

(cffi:defcfun ("imBinFileSize" %im-bin-file-size) :unsigned-long
  "Returns the file size in bytes."
  (bfile im-bin-file))

(cffi:defcfun ("imBinFileByteOrder" %im-bin-file-byte-order) :int
  "Changes the file byte order. Returns the old one."
  (bfile im-bin-file)
  (p-byte-order :int))

(cffi:defcfun ("imBinFileRead" %im-bin-file-read) :unsigned-long
  "Reads an array of count values with byte sizes: 1, 2, 4, 8 or 16. And
invert the byte order if necessary after read. Returns the actual count of
values read, or 0 if pSizeOf is not positive."
  (bfile im-bin-file)
  (p-values :pointer)
  (p-count :unsigned-long)
  (p-size-of :int))

(cffi:defcfun ("imBinFileWrite" %im-bin-file-write) :unsigned-long
  "Writes an array of values with sizes: 1, 2, 4, or 8. And invert the byte
order if necessary before write. <b>ATENTION</b>: The function will not
make a temporary copy of the values to invert the byte order. So after the
call the values will be invalid, if the file byte order is different from
the CPU byte order. Returns the actual count of values written, or 0 if
pSizeOf is not positive."
  (bfile im-bin-file)
  (p-values :pointer)
  (p-count :unsigned-long)
  (p-size-of :int))

;; REVIEW: unmapped C type(s) below; check against im_binfile.h

(cffi:defcfun ("imBinFilePrintf" %im-bin-file-printf) :unsigned-long
  "Writes a string without the NULL terminator. The function uses sprintf to
compose the string. The internal buffer is fixed at 10240 bytes. Returns
the actual count of values written."
  (bfile im-bin-file)
  (format :string)
  (arg2 :pointer #| ... |#))

(cffi:defcfun ("imBinFileReadLine" %im-bin-file-read-line) :int
  "Reads a line until line break. Returns the line in array, must have room
enough. Line break is discarded. Use *size to inform buffer size. *size
returns the number of bytes placed in the array, counting the zero it is
terminated with. An empty line returns a *size of zero and leaves the
array alone, so check it before reading one."
  (handle im-bin-file)
  (comment :pointer)
  (size :pointer))

(cffi:defcfun ("imBinFileSkipLine" %im-bin-file-skip-line) :int
  "Skips a line, including line break."
  (handle im-bin-file))

(cffi:defcfun ("imBinFileReadInteger" %im-bin-file-read-integer) :int
  "Reads an integer number from the current position until found a non
integer character. Returns a non zero value if successful."
  (handle im-bin-file)
  (value :pointer))

(cffi:defcfun ("imBinFileReadReal" %im-bin-file-read-real) :int
  "Reads an floating point number from the current position until found a non
number character. Returns a non zero value if successful."
  (handle im-bin-file)
  (value :pointer))

(cffi:defcfun ("imBinFileSeekTo" %im-bin-file-seek-to) :void
  "Moves the file pointer from the beginning of the file. When writing to a
file seeking can go beyond the end of the file."
  (bfile im-bin-file)
  (p-offset :unsigned-long))

(cffi:defcfun ("imBinFileSeekOffset" %im-bin-file-seek-offset) :void
  "Moves the file pointer from current position. If the offset is a negative
value the pointer moves backwards."
  (bfile im-bin-file)
  (p-offset :long))

(cffi:defcfun ("imBinFileSeekFrom" %im-bin-file-seek-from) :void
  "Moves the file pointer from the end of the file. The offset is usually a
negative value."
  (bfile im-bin-file)
  (p-offset :long))

(cffi:defcfun ("imBinFileTell" %im-bin-file-tell) :unsigned-long
  "Returns the current offset position."
  (bfile im-bin-file))

(cffi:defcfun ("imBinFileEndOfFile" %im-bin-file-end-of-file) :int
  "Indicates that the file pointer is at the end of the file."
  (bfile im-bin-file))

(cffi:defcfun ("imBinFileSetCurrentModule" %im-bin-file-set-current-module) :int
  "Sets the current I/O module. \\returns the previous function set, or -1 if
failed. A module is never -1, so that return unambiguously means the
module was rejected and the previous one is still in force. See also
imBinFileModule."
  (p-module :int))

(cffi:defcfun ("imBinMemoryRelease" %im-bin-memory-release) :void
  "Release the internal memory allocated when writing a Memory File (see
imBinMemoryFileName)."
  (buffer :pointer))

(cffi:defcfun ("imBinFileRegisterModule" %im-bin-file-register-module) :int
  (p-new-func :pointer))
