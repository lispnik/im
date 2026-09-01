;;;; src/cli/main.lisp — the top-level command and entry point.

(in-package #:im.cli)

(defparameter *program-version*
  ;; Asked of ASDF at LOAD time, which is the only point where the answer is
  ;; both available and current. Reading version.txt at compile time with #.
  ;; looked equivalent and was not: nothing tells ASDF that this file depends
  ;; on version.txt, so bumping the version left a valid cached fasl in place
  ;; and the rebuilt binary confidently reported the previous version.
  ;;
  ;; A top-level form runs on every load, cached fasl or not, and a dumped
  ;; image captures whatever it computed -- so the executable carries the
  ;; right string without needing version.txt beside it at runtime.
  (or (ignore-errors (asdf:component-version (asdf:find-system "im/cli")))
      "unknown")
  "The version of this tool, from the ASDF system definition.")

(define-condition usage-error (cl:error)
  ((text :initarg :text :reader usage-error-text))
  (:report (lambda (c s) (write-string (usage-error-text c) s)))
  (:documentation "The arguments were wrong, as opposed to the operation failing."))

(defun usage-error (control &rest arguments)
  (cl:error 'usage-error :text (apply #'format nil control arguments)))

(defun global-options ()
  "Options every subcommand inherits.

:PERSISTENT is what makes them reachable from a subcommand handler through
CLINGON:GETOPT, which walks up to the parent command."
  (list
   (clingon:make-option
    :flag :long-name "json" :short-name #\j :key :json :persistent t
    :description "Emit JSON instead of formatted text")
   (clingon:make-option
    :flag :long-name "verbose" :short-name #\v :key :verbose :persistent t
    :description "Report progress on stderr")
   (clingon:make-option
    :string :long-name "im-library" :key :im-library :persistent t
    :env-vars '("IM_LIBRARY_PATH")
    :description "Directory holding libim and its add-ons")))

(defun apply-global-options (command)
  "Read the persistent options into the specials the rest of the tool uses.

The library path is handled before anything touches an image: changing it
after the libraries are open would have no effect, and silently so."
  (setf *json* (clingon:getopt command :json)
        *verbose* (clingon:getopt command :verbose))
  (let ((path (clingon:getopt command :im-library)))
    ;; Reload only if the libraries are not already the ones being asked for.
    ;; The env var reaches SRC/LIBRARY.LISP on its own at load time, so acting
    ;; on it here reloaded a second time -- harmless for most libraries, but
    ;; re-registering the JP2 driver runs jas_init again and prints JasPer's
    ;; deprecation banner twice.
    (when (and path (plusp (length path))
               (not (already-loaded-from-p path)))
      (setf im:*library-path* path)
      (verbose "~&Loading IM from ~A~%" path)
      (im:load-libraries))))

(defun already-loaded-from-p (directory)
  "True when libim was demonstrably loaded from DIRECTORY.

NIL when that cannot be established, which includes the case that matters:
CFFI reports whatever it was asked to open, so a library found by search is
recorded under its bare soname -- \"libim.so\" -- and not as a path. TRUENAME
on that signals SIMPLE-FILE-ERROR rather than returning NIL, and because this
runs from APPLY-GLOBAL-OPTIONS before any subcommand does its work, it killed
every invocation of the binary on Linux with a file error naming a library in
the current directory.

Being wrong in the NIL direction costs one redundant reload. Signalling costs
the whole program, so everything here is guarded."
  (ignore-errors
   (let* ((loaded (im:library-pathname 'im::lib-im))
          (from (and loaded (uiop:truename* loaded)))
          (want (uiop:truename* directory)))
     (and from want
          (equal (uiop:ensure-directory-pathname want)
                 (uiop:pathname-directory-pathname from))))))

(defun top-level/handler (command)
  (clingon:print-usage-and-exit command *standard-output*))

(defun top-level-command ()
  (clingon:make-command
   :name "im"
   :version *program-version*
   :description "Inspect, convert, process and analyse images with the IM toolkit."
   :long-description
   "Every subcommand accepts --json, which turns its output into a single JSON
value suitable for jq. Formats, colour spaces and data types are named as the
Lisp API names them, so \"color-space-rgb\" and \"data-type-byte\".

  im info photo.jpg                    what is in this file
  im formats --compressions            what can be written, and how
  im convert in.jpg out.tif --compression LZW
  im process in.png out.png --op resize=800x600 --op sobel
  im analyze rice.png --regions
  im stats lena.jpg
  im compare a.png b.png
  im diff a.png b.png --output heatmap.png
  im montage *.jpg --output sheet.png --columns 4
  im capture --list

Images are read and written through IM, so the format list depends on which
add-ons the local build has: `im library' reports that."
   :authors '("Matthew Kennedy <burnsidemk@gmail.com>")
   :license "MIT"
   :options (global-options)
   :sub-commands (subcommands)
   :handler #'top-level/handler))

;;; Exit codes ----------------------------------------------------------------
;;;
;;;   0  success
;;;   1  an IM error, or any other unhandled failure
;;;   2  the command line did not make sense
;;; 130  interrupted

(defun guarded (handler)
  "Wrap a subcommand handler so its failures become exit codes and messages.

This has to happen INSIDE the handler rather than around CLINGON:RUN.
Clingon's own RUN establishes a HANDLER-CASE with a catch-all

    (error (condition) (format *error-output* \"~&~A~&\" condition) (exit 1))

and being the innermost handler it wins: an outer HANDLER-CASE in MAIN never
runs, so the `im: ' prefix never appeared and a usage error exited 1 where it
should exit 2. Wrapping each handler puts our handler further in than theirs."
  (lambda (command)
    (handler-case (funcall handler command)
      ;; `im info x.png | head' closes the pipe. A normal way to use the tool.
      (sb-int:broken-pipe ()
        (sb-ext:exit :code 0 :abort t))
      (sb-sys:interactive-interrupt ()
        (format *error-output* "~&Interrupted.~%")
        (sb-ext:exit :code 130))
      (usage-error (c)
        (format *error-output* "~&im: ~A~%" c)
        (sb-ext:exit :code 2))
      (im:im-error (c)
        (format *error-output* "~&im: ~A~%" c)
        (sb-ext:exit :code 1))
      (cl:error (c)
        (format *error-output* "~&im: ~A~%" c)
        (sb-ext:exit :code 1)))))

(defun main ()
  ;; The subcommand handlers are individually GUARDED, which is what actually
  ;; produces the exit codes -- clingon's own catch-all sits between here and
  ;; them. This outer HANDLER-CASE still covers what happens outside a
  ;; handler: building the command tree, and parsing failures clingon
  ;; re-signals rather than handling.
  (handler-case (clingon:run (top-level-command))
    ;; `im info x.png | head' closes the pipe. That is a normal way to use the
    ;; tool, not a failure worth a backtrace.
    (sb-int:broken-pipe ()
      (sb-ext:exit :code 0 :abort t))
    (sb-sys:interactive-interrupt ()
      (format *error-output* "~&Interrupted.~%")
      (sb-ext:exit :code 130))
    (usage-error (c)
      (format *error-output* "~&im: ~A~%" c)
      (sb-ext:exit :code 2))
    (im:library-not-found (c)
      ;; Worth its own arm: the fix is environmental, and the condition's
      ;; report already lists what was tried and names IM_LIBRARY_PATH.
      (format *error-output* "~&im: ~A~%" c)
      (sb-ext:exit :code 1))
    (im:im-error (c)
      (format *error-output* "~&im: ~A~%" c)
      (sb-ext:exit :code 1))
    (cl:error (c)
      (format *error-output* "~&im: ~A~%" c)
      (sb-ext:exit :code 1))))
