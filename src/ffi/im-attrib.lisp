;;;; src/ffi/im-attrib.lisp — DRAFTED by tools/gen-bindings.lisp.
;;;;
;;;; Source: im_attrib_flat.h
;;;; Hand corrections below this line are expected and are kept;
;;;; re-run the generator into a clean tree and diff.

(in-package #:im.ffi)

(cffi:defcfun ("imAttribTableCreate" %im-attrib-table-create) im-attrib-table
  (hash-size :int))

(cffi:defcfun ("imAttribTableDestroy" %im-attrib-table-destroy) :void
  (ptable im-attrib-table))

(cffi:defcfun ("imAttribTableCount" %im-attrib-table-count) :int
  (ptable im-attrib-table))

(cffi:defcfun ("imAttribTableRemoveAll" %im-attrib-table-remove-all) :void
  (ptable im-attrib-table))

(cffi:defcfun ("imAttribTableGet" %im-attrib-table-get) :pointer
  (ptable im-attrib-table)
  (name :string)
  (data-type :pointer)
  (count :pointer))

(cffi:defcfun ("imAttribTableGetInteger" %im-attrib-table-get-integer) :int
  (ptable im-attrib-table)
  (name :string)
  (index :int))

(cffi:defcfun ("imAttribTableGetReal" %im-attrib-table-get-real) :double
  (ptable im-attrib-table)
  (name :string)
  (index :int))

(cffi:defcfun ("imAttribTableGetString" %im-attrib-table-get-string) :string
  (ptable im-attrib-table)
  (name :string))

(cffi:defcfun ("imAttribTableSet" %im-attrib-table-set) :void
  (ptable im-attrib-table)
  (name :string)
  (data-type :int)
  (count :int)
  (data :pointer))

(cffi:defcfun ("imAttribTableSetInteger" %im-attrib-table-set-integer) :void
  (ptable im-attrib-table)
  (name :string)
  (data-type :int)
  (value :int))

(cffi:defcfun ("imAttribTableSetReal" %im-attrib-table-set-real) :void
  (ptable im-attrib-table)
  (name :string)
  (data-type :int)
  (value :double))

(cffi:defcfun ("imAttribTableSetString" %im-attrib-table-set-string) :void
  (ptable im-attrib-table)
  (name :string)
  (value :string))

(cffi:defcfun ("imAttribTableUnSet" %im-attrib-table-un-set) :void
  (ptable im-attrib-table)
  (name :string))

(cffi:defcfun ("imAttribTableCopyFrom" %im-attrib-table-copy-from) :void
  (ptable-dst im-attrib-table)
  (ptable-src im-attrib-table))

(cffi:defcfun ("imAttribTableMergeFrom" %im-attrib-table-merge-from) :void
  (ptable-dst im-attrib-table)
  (ptable-src im-attrib-table))

(cffi:defcfun ("imAttribTableForEach" %im-attrib-table-for-each) :void
  (ptable im-attrib-table)
  (user-data :pointer)
  (attrib-func :pointer))

(cffi:defcfun ("imAttribArrayCreate" %im-attrib-array-create) im-attrib-table
  (hash-size :int))

(cffi:defcfun ("imAttribArrayGet" %im-attrib-array-get) :pointer
  (ptable im-attrib-table)
  (index :int)
  (name :pointer)
  (data-type :pointer)
  (count :pointer))

(cffi:defcfun ("imAttribArraySet" %im-attrib-array-set) :void
  (ptable im-attrib-table)
  (index :int)
  (name :string)
  (data-type :int)
  (count :int)
  (data :pointer))

(cffi:defcfun ("imAttribArrayCopyFrom" %im-attrib-array-copy-from) :void
  (ptable-dst im-attrib-table)
  (ptable-src im-attrib-table))
