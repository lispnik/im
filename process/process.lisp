(defpackage #:im-process
  (:use #:common-lisp
	#:cffi)
  (:import-from #:im-process-cffi #:counter-aborted)
  (:import-from #:tecgraf-base #:defalias)
  (:export #:counter-aborted))

(in-package #:im-process)

;;; Lua bindings have a nice template for how to wrap up the
;;; processing functions nicely
;;; 
;;; http://svn.code.sf.net/p/imtoolkit/im/trunk/im/src/lua5/im_process.lua
