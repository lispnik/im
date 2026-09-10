;;;; src/ffi-structs.lisp — public C structs and callback types, hand-written.
;;;;
;;;; Deliberately NOT in src/ffi/. That directory belongs to
;;;; tools/gen-bindings.lisp: regenerating clears it, and a hand-written file
;;;; kept there is a hand-written file waiting to be deleted. Anything in the
;;;; IM.FFI package that a human maintains lives here instead.
;;;;
;;;; The generator reads function declarations. Getting a struct right needs
;;;; the field comments and, for imStats, a judgement about the C type model
;;;; that no amount of parsing supplies.

(in-package #:im.ffi)

;;; imImage — im_image.h:37
;;;
;;; Read through rather than cached. The previous binding's wrappers took
;;; width and height once; imImageReshape rewrites them in place, so anything
;;; holding a copy was wrong from that call onward.
;;;
;;; Note what the field comments promise, because the high-level layer relies
;;; on it: planes are ALWAYS unpacked and the orientation is ALWAYS bottom-up,
;;; data[0] aliases the entire buffer, and data[i] = data[0] + i*plane_size.

(cffi:defcstruct im-image-struct
  (width :int)
  (height :int)
  (color-space :int)
  (data-type :int)
  (has-alpha :int)
  ;; Secondary parameters, all derived by IM from the five above, and
  ;; recomputed by IM on every reshape.
  (depth :int)
  (line-size :int)
  (plane-size :int)
  (size :int)
  (count :int)
  (data :pointer)             ; void** — array of plane pointers
  (palette :pointer)          ; long*  — 256 allocated, PALETTE-COUNT used
  (palette-count :int)
  (attributes-table :pointer))

;;; imStats — im_process_ana.h:121
;;;
;;; The three counters are `unsigned long`, which is where this struct stops
;;; being portable: 8 bytes under LP64 (Linux, macOS) and 4 under LLP64
;;; (64-bit Windows). CFFI's :unsigned-long already follows the platform, so
;;; naming the C type rather than a fixed width is not merely acceptable here,
;;; it is the only spelling that is right on both. Substituting :uint64 --
;;; which is what "unsigned long is 64-bit" would suggest to anyone who has
;;; only used Unix -- shifts MEAN and STDDEV by eight bytes on Windows, and
;;; what comes back is garbage rather than an error.
;;;
;;; Returned as an array with one element per plane, so reading plane N means
;;; MEM-AREF with this struct type, not MEM-REF.

(cffi:defcstruct im-stats-struct
  (max :double)
  (min :double)
  (positive :unsigned-long)
  (negative :unsigned-long)
  (zeros :unsigned-long)
  (mean :double)
  (stddev :double))

;;; imBinMemoryFileName — im_binfile.h:151
;;;
;;; The in-memory I/O path: passed where a filename would go when the current
;;; binfile module is :BIN-FILE-MODULE-MEMFILE. BUFFER is malloc'd by IM when
;;; writing and must be released with imBinMemoryRelease, not by anything that
;;; knows about the Lisp heap.

(cffi:defcstruct im-bin-memory-file-name
  (buffer :pointer)
  (size :int)
  ;; Growth factor when writing past the end. Upstream's fork note: a factor
  ;; too small to add a whole byte now grows by one byte rather than stalling;
  ;; negative disables reallocation entirely.
  (reallocate :float))

;;; imDecorrelationTransform — im_process_pnt.h:490
;;;
;;; The decorrelation stretch, computed once by
;;; imProcessDecorrelationCalcTransform and applied any number of times by
;;; imProcessDecorrelationApplyTransform. Passed by pointer both ways, and
;;; plain old data throughout, so a WITH-FOREIGN-OBJECT is the whole of the
;;; lifetime management.
;;;
;;; Unlike imStats above there is no C type model to get wrong here: every
;;; field is a double or an int, both fixed width on every platform IM
;;; builds on, so the obvious spelling is also the portable one.
;;;
;;; MATRIX and OFFSET are the transform, and the only fields APPLY reads:
;;; out = MATRIX * in + OFFSET, in the source's own components. The rest is
;;; what the calculation found and is there to be looked at -- RANK below 3
;;; means the colours were confined to a plane or a line, which is the case
;;; where the stretch cannot do its whole job and says so.

(cffi:defcstruct im-decorrelation-transform-struct
  (matrix :double :count 9)   ; row-major 3x3
  (offset :double :count 3)
  (mean :double :count 3)
  (target :double :count 3)
  (stddev :double :count 3)
  (rank :int)
  (color-space :int))
