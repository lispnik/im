;;;; src/ffi/im-lib.lisp — DRAFTED by tools/gen-bindings.lisp.
;;;;
;;;; Source: im_lib.h
;;;; Hand corrections below this line are expected and are kept;
;;;; re-run the generator into a clean tree and diff.

(in-package #:im.ffi)

(cffi:defcfun ("imVersion" %im-version) :string
  "Returns the library current version. Returns the definition IM_VERSION
plus the bug fix number.")

(cffi:defcfun ("imVersionDate" %im-version-date) :string
  "Returns the library current version release date. Returns the definition
IM_VERSION_DATE.")

(cffi:defcfun ("imVersionNumber" %im-version-number) :int
  "Returns the library current version number. Returns the definition
IM_VERSION_NUMBER plus the bug fix number. Can be compared in run time
with IM_VERSION_NUMBER to compare compiled and linked versions of the
library.")
