;;;; src/package.lisp — package definitions.
;;;;
;;;; Two packages carry the binding, replacing the twenty flat ones the
;;;; previous version exposed (IM, IM-IMAGE, IM-FILE, IM-ARITHMETIC,
;;;; IM-CONVOLVE, IM-MORPH, ...).
;;;;
;;;; That split mirrored IM's C header layout rather than anything a caller
;;;; cares about, and it cost every call site a package qualifier chosen from
;;;; a list nobody can hold in their head -- IM-GEOMETRIC:RESIZE but
;;;; IM-CONVOLVE:SOBEL but IM-RENDER:GAUSSIAN, with IM-RENDER living in
;;;; synthetic.lisp and IM-CALC in statistics.lisp for good measure. It also
;;;; forced eight packages to shadow a CL symbol (OPEN, CLOSE, COUNT,
;;;; SEQUENCE, RESHAPE, WITH-OPEN-FILE), so callers who did :USE one of them
;;;; got conflicts.
;;;;
;;;; Here the operation family lives in the symbol name instead of the package
;;;; name -- IM:CONVOLVE-SOBEL, IM:MORPH-ERODE, IM:RENDER-GAUSSIAN -- which
;;;; reads the same at a REPL, in a file that :USEs nothing, and on a CLI
;;;; option name.

(defpackage #:im.ffi
  (:use #:common-lisp)
  (:documentation
   "Raw CFFI bindings: one file per upstream header, under src/ffi/.

Everything here is internal. The names are mechanical transcriptions of the C
ones (%IM-FILE-OPEN for imFileOpen) and the argument types are the C types, so
callers get foreign pointers, ints and out-parameters rather than Lisp values.
The IM package reaches in with double colons; nothing else should.

Files under src/ffi/ are first drafted by tools/gen-bindings.lisp and then
hand-corrected. See that file for what the generator can and cannot be
trusted with."))

(defpackage #:im
  (:use #:common-lisp)
  (:documentation
   "Common Lisp bindings to IM, Tecgraf's imaging toolkit.

Images are CLOS objects (IM:IMAGE) whose foreign storage is released by
IM:DESTROY, by IM:WITH-IMAGE on unwind, or as a last resort by a finalizer.
Failures signal subtypes of IM:IM-ERROR; long-running operations establish
RETRY and CONTINUE restarts and can be cancelled from a progress callback
installed with IM:WITH-PROGRESS.")
  ;; Exactly one shadow. IM:LOAD reading an image from a file is the obvious
  ;; spelling and worth the collision; CL:LOAD is still reachable by prefix
  ;; and this binding has no reason to call it.
  ;;
  ;; Nothing else is shadowed on purpose. The previous version shadowed a CL
  ;; symbol in eight of its twenty packages, and shadowing ERROR in particular
  ;; is a trap: every CL:ERROR call inside the package then has to be written
  ;; out, and the one that isn't fails at the worst moment. Where a good name
  ;; collides, it gets a prefix instead -- IM:IM-ERROR, IM:DATA-TYPE,
  ;; IM:FRAME-COUNT.
  (:shadow #:load))
