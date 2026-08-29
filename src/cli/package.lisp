;;;; src/cli/package.lisp — the im(1) command-line tool.

(defpackage #:im.cli
  (:use #:common-lisp)
  (:export #:main)
  (:documentation
   "A command-line driver for the whole binding.

Structured the way this author's other tools are: each subcommand lives in its
own file, builds a clingon command, and registers itself at load time.
TOP-LEVEL-COMMAND reads the registry inside MAIN rather than at load time, so
the files can load in any order."))

(in-package #:im.cli)

(defvar *subcommands* nil)

(defun register-subcommand (command)
  (pushnew command *subcommands*
           :test (lambda (a b) (equal (clingon:command-name a)
                                      (clingon:command-name b)))))

(defun subcommands ()
  "The registered subcommands, in a stable alphabetical order.

PUSHNEW leaves them in reverse load order, which is neither alphabetical nor
meaningful, and which would reshuffle the help output whenever a file moved."
  (sort (copy-list *subcommands*) #'string< :key #'clingon:command-name))
