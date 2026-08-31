;;;; src/display.lisp — showing an image in the editor attached to this Lisp.
;;;;
;;;; The image is written to a PNG in a temporary directory and the front end
;;;; is told to show that file. Neither SLIME nor SLY can be handed pixels
;;;; directly -- their wire protocol carries text -- and both already know how
;;;; to put an image file on screen, so the file is the whole trick.
;;;;
;;;; Nothing here is compiled against SWANK or SLYNK. Neither is a dependency
;;;; of this system and neither should become one for the sake of a REPL
;;;; convenience, so the symbols are looked up at call time: this file loads
;;;; and behaves the same in a bare SBCL.

(in-package #:im)

(export '(display
          *display-function*
          *display-directory*
          *display-history*))

(defvar *display-function* nil
  "A function of (IMAGE PATHNAME) that DISPLAY calls instead of its backends.

Bind this to teach DISPLAY about a front end it does not know: a terminal that
draws inline images, a notebook, a test harness. PATHNAME is the PNG DISPLAY
has already written. Return a name for the front end -- it becomes DISPLAY's
second value -- or NIL to say the image could not be shown, which DISPLAY
reports as DISPLAY-UNAVAILABLE.")

(defvar *display-directory* nil
  "Where DISPLAY writes its PNGs, or NIL for a per-process temporary directory.")

(defvar *display-history* 8
  "How many of DISPLAY's PNGs to keep on disk.

Not zero, and not one: the front end reads the file after DISPLAY has
returned, so the file it was just handed cannot be deleted yet.")

(defvar *display-counter* 0)

(defvar *display-session* nil
  "A token keeping two Lisp processes from writing over each other's PNGs.

A process id would be the obvious thing, and UIOP has no portable one -- the
ASDF that SBCL ships exports no GETPID -- so this is a random token, computed
once and reused for the life of the process.")

(defvar *display-buffer-name* "*im-image*"
  "The Emacs buffer SLY displays into. SLIME inserts inline instead.")

(defun %display-session ()
  (or *display-session*
      (setf *display-session*
            (format nil "~36R" (random (expt 36 8) (make-random-state t))))))

(defun %display-directory ()
  (or *display-directory*
      (merge-pathnames (format nil "im-display-~A/" (%display-session))
                       (uiop:temporary-directory))))

(defun %next-display-file ()
  "A fresh PNG pathname, with the oldest of the previous ones deleted.

Fresh rather than reused because Emacs caches images by their spec, and a
second image written to the same path shows the first one again."
  (let* ((directory (%display-directory))
         (n (incf *display-counter*))
         (path (merge-pathnames (format nil "image-~D.png" n) directory)))
    (ensure-directories-exist path)
    (let ((old (merge-pathnames (format nil "image-~D.png" (- n *display-history*))
                                directory)))
      (when (probe-file old)
        (ignore-errors (delete-file old))))
    path))

;;; Front ends ----------------------------------------------------------------

(defun %attached-p (package-name)
  "True when PACKAGE-NAME's server has a live Emacs connection in this thread.

Both SWANK and SLYNK bind *EMACS-CONNECTION* per request, so this is also the
answer to \"is this thread one Emacs is waiting on\" -- which is the question
that matters, since a background thread has nowhere to send an event."
  (let ((package (find-package package-name)))
    (when package
      (let ((symbol (find-symbol "*EMACS-CONNECTION*" package)))
        (and symbol (boundp symbol) (symbol-value symbol) t)))))

(defun %external-symbol-function (package-name symbol-name)
  "The function named PACKAGE-NAME:SYMBOL-NAME, or NIL if there is no such
function loaded."
  (let ((package (find-package package-name)))
    (when package
      (let ((symbol (find-symbol symbol-name package)))
        (when (and symbol (fboundp symbol))
          (symbol-function symbol))))))

(defun %emacs-display-form (pathname label)
  "The Emacs form that shows PATHNAME.

Kept to lists, strings, symbols and numbers: SLYNK serialises it with
PROCESS-FORM-FOR-EMACS, whose ETYPECASE knows those four and nothing else."
  `(with-current-buffer (get-buffer-create ,*display-buffer-name*)
     (let ((inhibit-read-only t))
       (erase-buffer)
       (insert-image (create-image ,(namestring pathname)) ,label)
       (goto-char (point-min)))
     (display-buffer (current-buffer))
     nil))

(defun %sly-display (pathname label)
  "Show PATHNAME under SLY, or NIL when SLY is not attached here.

Sent with SLYNK's NOWAIT, which is not the obvious choice and is the safe one.
The blocking form waits for Emacs to return a value, and Emacs checks
`sly-enable-evaluate-in-emacs' in the event dispatcher, outside the handler
that turns an error into that return -- so with the option at its default NIL,
waiting means waiting forever. Not waiting costs the confirmation and gives
the user a legible error in Emacs instead of a wedged REPL."
  (let ((eval-in-emacs (%external-symbol-function '#:slynk "EVAL-IN-EMACS")))
    (when (and eval-in-emacs (%attached-p '#:slynk))
      (funcall eval-in-emacs (%emacs-display-form pathname label) t)
      :sly)))

(defun %swank-image-type (name)
  "NAME as a symbol Emacs will read as a bare, lowercase one.

The obvious :PNG is wrong, and wrong quietly: an Emacs image spec holds a
plain symbol, FIND-IMAGE looks it up with MEMQ against `image-types', and a
keyword matches nothing there -- slime-media then falls through to a
CREATE-IMAGE call that cannot work either, so the image just never appears.

SWANK prints everything it sends with *PACKAGE* bound to its own
SWANK-IO-PACKAGE and *PRINT-CASE* :DOWNCASE, so a symbol interned in that
package arrives on the other side as `png' and nothing else. NIL if this
SWANK does not have such a package, in which case there is no way to name an
image type and the backend declines rather than sending a spec Emacs will
drop on the floor."
  (let ((package (find-symbol "*SWANK-IO-PACKAGE*" '#:swank)))
    (when (and package (boundp package) (packagep (symbol-value package)))
      (intern name (symbol-value package)))))

(defun %slime-display (pathname label)
  "Show PATHNAME under SLIME, or NIL when SLIME is not attached here.

:WRITE-IMAGE is the slime-media contrib's event, and its argument is a list of
Emacs image specs -- what FIND-IMAGE takes -- not a single spec. LABEL is the
text the image stands in for, and what a caller sees where Emacs cannot render
a PNG."
  (let ((send-to-emacs (%external-symbol-function '#:swank "SEND-TO-EMACS"))
        (png (%swank-image-type "PNG")))
    (when (and send-to-emacs png (%attached-p '#:swank))
      (funcall send-to-emacs
               (list :write-image
                     (list (list :type png :file (namestring pathname)))
                     label))
      :slime)))

;;; -----------------------------------------------------------------------------

(defun display (image &key pathname (format "PNG"))
  "Show IMAGE in the editor attached to this Lisp. Returns IMAGE.

The second value names the front end that was used: :SLY, :SLIME, or whatever
*DISPLAY-FUNCTION* returned.

Each front end wants one thing set up on the Emacs side, and neither is
something this side can check:

  SLY     needs `sly-enable-evaluate-in-emacs' set to T. The image appears in
          an *im-image* buffer.
  SLIME   needs the slime-media contrib -- (slime-setup '(slime-media)) --
          and the image is inserted inline as the REPL result.

Both are one-way messages, so a missing prerequisite surfaces as an error in
Emacs rather than as a condition here. That is the reason for the two lines
above: there is nowhere else the failure can be reported.

With nothing attached -- a bare REPL, a script, a thread the editor does not
know about -- this signals DISPLAY-UNAVAILABLE rather than quietly writing a
file no one will look at.

PATHNAME writes the image somewhere specific instead of to a temporary file
that DISPLAY later cleans up; FORMAT is for a front end that wants something
other than PNG."
  (let ((path (or pathname (%next-display-file))))
    (save image path :format format)
    (let* ((label (prin1-to-string image))
           (backend (if *display-function*
                        (funcall *display-function* image path)
                        (or (%sly-display path label)
                            (%slime-display path label)))))
      (unless backend
        (cl:error 'display-unavailable :detail (namestring path)))
      (values image backend))))
