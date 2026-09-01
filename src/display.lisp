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
  "Where DISPLAY writes its images, or NIL for a per-process temporary one.

DISPLAY also deletes from this directory, so what it may delete is worth being
precise about: every file it writes is named im-<session>-<n>.<ext>, where
<session> is unique to this process, and the sweep only ever considers that
prefix. Point this at a directory of your own and nothing already in it is
touched.")

(defvar *display-history* 8
  "How many of DISPLAY's images to keep on disk.

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
  "Where to write, always as a directory pathname.

*DISPLAY-DIRECTORY* is normalised rather than used as given: #p\"/tmp/shots\"
has \"shots\" as its NAME, so MERGE-PATHNAMES would replace it and the PNGs
would land in /tmp -- where the history sweep would then delete files nobody
nominated."
  (uiop:ensure-directory-pathname
   (or *display-directory*
       (merge-pathnames (format nil "im-display-~A/" (%display-session))
                        (uiop:temporary-directory)))))

(defparameter *emacs-image-types*
  '(("png"  . "PNG")
    ("jpg"  . "JPEG") ("jpeg" . "JPEG")
    ("tif"  . "TIFF") ("tiff" . "TIFF")
    ("gif"  . "GIF"))
  "Filename extension to the name of the Emacs image type symbol.

Emacs compares image types with MEMQ against `image-types', so a format absent
from this table has no spelling %SLIME-DISPLAY can be sure of and it declines
rather than send one Emacs will fail to match -- silently, which is the whole
failure mode this table exists to avoid.")

(defun %display-extension (format)
  "The filename extension an IM format should be written under.

From *EXTENSION-FORMATS*, the table SAVE guesses with, so the file's name says
what is actually in it. Naming JPEG bytes .png is not cosmetic here: the SLIME
backend takes the image type from the extension, and Emacs would be told to
read a PNG that is not one."
  (or (car (rassoc format *extension-formats* :test #'string=))
      (string-downcase format)))

(defun %display-file-prefix ()
  "The prefix every file this process writes shares, and the only one it sweeps."
  (format nil "im-~A-" (%display-session)))

(defun %next-display-file (extension)
  "An image pathname nothing else holds, with older ones swept away.

Fresh rather than reused because Emacs caches images by their spec: a second
image written to the same path shows the first one again.

The name is claimed by creating the file, not by incrementing the counter.
INCF is three operations, so two threads -- or two Lisps sharing a directory --
can be handed the same number, and the loser's image is overwritten while
Emacs may be part way through reading it. :IF-EXISTS NIL settles that where it
is actually decided, in the filesystem."
  (let ((directory (%display-directory))
        (prefix (%display-file-prefix)))
    (ensure-directories-exist directory)
    (loop for n = (incf *display-counter*)
          for path = (merge-pathnames (format nil "~A~D.~A" prefix n extension)
                                      directory)
          for stream = (open path :direction :output
                                  :if-does-not-exist :create :if-exists nil)
          when stream
            do (close stream)
               (%sweep-display-directory directory prefix n)
               (return path))))

(defun %sweep-display-directory (directory prefix newest)
  "Delete this process's own images in DIRECTORY beyond *DISPLAY-HISTORY*.

Its own, by PREFIX. The sweep used to match image-<n>.png and delete anything
that fit, which is a fine rule for a directory it made and a data-loss bug for
the one a user nominates: *DISPLAY-COUNTER* starts at zero each process, so the
first call steps past whatever files are already there and then reaps the ones
it stepped over. A directory seeded with twelve image-<n>.png files lost five
of them to a single DISPLAY call.

By number rather than by one fixed offset per call, too: lowering
*DISPLAY-HISTORY* or moving *DISPLAY-DIRECTORY* would otherwise strand every
file the old offset had already stepped past, for the life of the machine."
  (let ((limit (- newest *display-history*)))
    (when (plusp limit)
      (dolist (file (ignore-errors
                     (directory (merge-pathnames (format nil "~A*.*" prefix)
                                                 directory))))
        (let ((n (ignore-errors (parse-integer (pathname-name file)
                                               :start (length prefix)))))
          (when (and n (<= n limit))
            (ignore-errors (delete-file file))))))))

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

(defun %loaded-symbol-function (package-name symbol-name)
  "The function PACKAGE-NAME::SYMBOL-NAME names, or NIL if it is not loaded.

Internal symbols included -- SEND-TO-EMACS and EVAL-IN-EMACS are internal to
their packages, so a search restricted to external ones would find neither."
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
  (let ((eval-in-emacs (%loaded-symbol-function '#:slynk "EVAL-IN-EMACS")))
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
  ;; FIND-PACKAGE first, and not only for tidiness: FIND-SYMBOL signals a
  ;; PACKAGE-ERROR when its designator names no package, so probing for a
  ;; SWANK that was never loaded is itself the crash -- in the bare REPL this
  ;; backend exists to decline politely in.
  (let* ((swank (find-package '#:swank))
         (symbol (and swank (find-symbol "*SWANK-IO-PACKAGE*" swank))))
    (when (and symbol (boundp symbol) (packagep (symbol-value symbol)))
      (intern name (symbol-value symbol)))))

(defun %slime-display (pathname label)
  "Show PATHNAME under SLIME, or NIL when SLIME is not attached here.

:WRITE-IMAGE is the slime-media contrib's event, and its argument is a list of
Emacs image specs -- what FIND-IMAGE takes -- not a single spec. LABEL is the
text the image stands in for, and what a caller sees where Emacs cannot render
a PNG."
  (let* ((send-to-emacs (%loaded-symbol-function '#:swank "SEND-TO-EMACS"))
         ;; From the file's own extension, not from a hardcoded png: DISPLAY
         ;; can be asked for another format, and telling Emacs that JPEG bytes
         ;; are a PNG is the same silent non-render as sending it a keyword.
         (name (cdr (assoc (string-downcase (or (pathname-type pathname) ""))
                           *emacs-image-types* :test #'string=)))
         (type (and name (%swank-image-type name))))
    (when (and send-to-emacs type (%attached-p '#:swank))
      (funcall send-to-emacs
               (list :write-image
                     (list (list :type type :file (namestring pathname)))
                     label))
      :slime)))

;;; -----------------------------------------------------------------------------

(defun display (image &key pathname format)
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
that DISPLAY later cleans up, and its extension chooses the format -- naming a
file .jpg and getting PNG bytes in it helps nobody. FORMAT overrides that, and
is what the temporary file uses, where there is no extension to read."
  (let* ((format (or format (and (null pathname) "PNG")))
         (path (or pathname (%next-display-file (%display-extension format)))))
    (save image path :format format)
    (let* ((label (prin1-to-string image))
           (backend (if *display-function*
                        (funcall *display-function* image path)
                        (or (%sly-display path label)
                            (%slime-display path label)))))
      (unless backend
        (cl:error 'display-unavailable :detail (namestring path)))
      (values image backend))))
