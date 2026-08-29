;;;; src/library.lisp — finding, opening and re-opening the IM shared libraries.
;;;;
;;;; Three problems this file exists to solve, all of which the previous
;;;; version got wrong:
;;;;
;;;; 1. Discovery. It hardcoded four absolute paths, three of them under
;;;;    /Users/mkennedy/tecgraf/, into CFFI:*FOREIGN-LIBRARY-DIRECTORIES* at
;;;;    load time. That works on exactly one machine.
;;;;
;;;; 2. Dumped images. It called CFFI:USE-FOREIGN-LIBRARY at load time and
;;;;    stopped there. CFFI's record of a library as open survives
;;;;    SAVE-LISP-AND-DIE, so in a restored image USE-FOREIGN-LIBRARY
;;;;    short-circuits and never dlopens anything -- leaving the process bound
;;;;    to whatever the loader happened to pull in at startup, by soname,
;;;;    which on macOS resolves through the dyld shared cache. A dumped CLI
;;;;    would then silently disagree with the same code run from source.
;;;;
;;;; 3. Symbol collisions. Three pairs of IM libraries export the same names:
;;;;    im_process / im_process_omp are symbol-for-symbol identical, and
;;;;    im_fftw3 redefines six of im_process's functions. Loading both halves
;;;;    of a pair and letting dlsym pick is not a decision, it is a coin toss
;;;;    that depends on load order. See *PROCESS-LIBRARY* and FFT-SYMBOL below.

(in-package #:im)

(export '(*library-path*
          *process-library*
          library-pathname
          library-loaded-p
          loaded-libraries
          load-libraries
          fftw3-available-p))

(defvar *library-path* nil
  "Directory to load the IM shared libraries from, or NIL to search.

Overrides every other candidate when set. The IM_LIBRARY_PATH environment
variable does the same thing and is read at load time, which is what lets a
release tarball and a development checkout use the same binary.")

(defvar *process-library* :auto
  "Which build of the processing library to open.

  :AUTO    -- prefer im_process_omp, fall back to im_process (the default)
  :OPENMP  -- require im_process_omp
  :SERIAL  -- require im_process

im_process and im_process_omp export identical symbol sets, so exactly one of
them may be open at a time. The previous version chose between them with a
read-time #+im-process-use-openmp feature that nothing ever pushed, so the
OpenMP build was unreachable.")

;;; Library definitions -------------------------------------------------------
;;;
;;; Windows DLL names carry no lib prefix, and upstream's CMake gives the
;;; Unix builds no SOVERSION, so the bare .so name is correct there.

(cffi:define-foreign-library lib-im
  (:darwin "libim.dylib")
  (:unix "libim.so")
  (:windows "im.dll")
  (t (:default "im")))

(cffi:define-foreign-library lib-im-process
  (:darwin "libim_process.dylib")
  (:unix "libim_process.so")
  (:windows "im_process.dll")
  (t (:default "im_process")))

(cffi:define-foreign-library lib-im-process-omp
  (:darwin "libim_process_omp.dylib")
  (:unix "libim_process_omp.so")
  (:windows "im_process_omp.dll")
  (t (:default "im_process_omp")))

(cffi:define-foreign-library lib-im-fftw3
  (:darwin "libim_fftw3.dylib")
  (:unix "libim_fftw3.so")
  (:windows "im_fftw3.dll")
  (t (:default "im_fftw3")))

(cffi:define-foreign-library lib-im-jp2
  (:darwin "libim_jp2.dylib")
  (:unix "libim_jp2.so")
  (:windows "im_jp2.dll")
  (t (:default "im_jp2")))

(cffi:define-foreign-library lib-im-heif
  (:darwin "libim_heif.dylib")
  (:unix "libim_heif.so")
  (:windows "im_heif.dll")
  (t (:default "im_heif")))

(cffi:define-foreign-library lib-im-capture
  (:darwin "libim_capture.dylib")
  (:unix "libim_capture.so")
  (:windows "im_capture.dll")
  (t (:default "im_capture")))

;;; AVI and WMV are Windows-only format drivers, built on the imDib* family
;;; which itself exists only in a Windows libim. Upstream gained CMake targets
;;; for them only recently, and the WMV one additionally needs the separately
;;; distributed Windows Media Format SDK, so both are treated as optional even
;;; on Windows.

(cffi:define-foreign-library lib-im-avi
  (:windows "im_avi.dll")
  (t (:default "im_avi")))

(cffi:define-foreign-library lib-im-wmv
  (:windows "im_wmv.dll")
  (t (:default "im_wmv")))

(defparameter *core-libraries* '(lib-im)
  "Libraries without which the binding cannot function at all.")

(defparameter *optional-libraries*
  '(lib-im-fftw3 lib-im-jp2 lib-im-heif lib-im-capture
    #+windows lib-im-avi #+windows lib-im-wmv)
  "Add-ons that are absent from many builds.

Every one of these is gated behind a build option upstream -- IM_BUILD_HEIF
and IM_BUILD_CAPTURE default to OFF, JP2 is switched off in the Linux CI
because libjasper was dropped from Debian -- so a missing one is the normal
case, not a failure. Calls into an add-on that did not load fail with an
undefined-alien-function error at the point of use, which names the function
and is more useful than refusing to load the system at all.")

(defvar *loaded* (make-hash-table :test #'eq)
  "Maps a library designator to the namestring CFFI reported for it.")

(defun library-pathname (designator)
  "Where DESIGNATOR was loaded from, or NIL if it is not open."
  (gethash designator *loaded*))

(defun library-loaded-p (designator)
  (nth-value 1 (gethash designator *loaded*)))

(defun loaded-libraries ()
  "An alist of (DESIGNATOR . PATHNAME) for every IM library currently open."
  (let (result)
    (maphash (lambda (k v) (push (cons k v) result)) *loaded*)
    (nreverse result)))

(defun fftw3-available-p ()
  "True when libim_fftw3 is open, so FFT runs on FFTW3 rather than IM's own."
  (library-loaded-p 'lib-im-fftw3))

;;; Discovery -----------------------------------------------------------------

(defun %executable-library-directories ()
  "Directories to search relative to this executable, nearest first.

Two layouts, because the platforms want different ones:

  <exedir>/          everything in one directory. This is what a Windows
                     bundle has to look like: im.dll depends on tiff.dll and
                     the rest, and when a DLL is loaded by path Windows
                     resolves ITS dependencies against the directory of the
                     running executable, not against the directory the DLL
                     came from. Anything else and im.dll is found but will
                     not load.

  <exedir>/../lib/   bin/im beside lib/libim.so, which is the tidier shape
                     where the loader cooperates.

Both are checked, so one binary works with either. Returns NIL when running
from source, where argv[0] is the Lisp itself and neither exists."
  (let ((argv0 (ignore-errors (uiop:argv0))))
    (when argv0
      (let* ((exe (ignore-errors (uiop:truename* argv0)))
             (dir (when exe (uiop:pathname-directory-pathname exe))))
        (when dir
          (remove nil
                  (list (uiop:truename* dir)
                        (let ((lib (uiop:merge-pathnames* #p"../lib/" dir)))
                          (when (uiop:directory-exists-p lib)
                            (uiop:truename* lib))))))))))

(defun %search-directories ()
  "Directories to look in, most specific first.

Deliberately does not include a guess at anyone's home directory. The previous
version's list of /Users/mkennedy/tecgraf/... paths made this system load on
one machine and fail everywhere else, and the failure looked like a missing
library rather than a wrong assumption."
  (remove nil
          (append (list (when *library-path*
                          (uiop:ensure-directory-pathname *library-path*))
                        (let ((env (uiop:getenv "IM_LIBRARY_PATH")))
                          (when (and env (plusp (length env)))
                            (uiop:ensure-directory-pathname env))))
                  (%executable-library-directories))))

(defparameter *library-basenames*
  '((lib-im             . "im")
    (lib-im-process     . "im_process")
    (lib-im-process-omp . "im_process_omp")
    (lib-im-fftw3       . "im_fftw3")
    (lib-im-jp2         . "im_jp2")
    (lib-im-heif        . "im_heif")
    (lib-im-capture     . "im_capture")
    (lib-im-avi         . "im_avi")
    (lib-im-wmv         . "im_wmv"))
  "Designator to the C library's base name, as it appears on disk.

Kept beside the DEFINE-FOREIGN-LIBRARY forms above and has to move with them.
The alternative -- asking CFFI what a library is called -- does not work from a
designator: CFFI:FOREIGN-LIBRARY-NAME is a slot reader on a library object,
not on the symbol naming it, so calling it on a symbol signals
NO-APPLICABLE-METHOD. It did, from inside the handler whose whole job was to
report that the library could not be found, replacing the useful message with
a CLOS error and stopping %REINITIALIZE from catching LIBRARY-NOT-FOUND at
all.")

(defun %candidate-names (designator)
  "The file names CFFI would try for DESIGNATOR on this platform.

Used only to build the report on LIBRARY-NOT-FOUND. A message listing what was
actually tried is the one thing that turns \"cannot load the IM library\" into
something the reader can go and check -- so it has to name libim.dylib, not
liblib-im.dylib, which is what interpolating the Lisp designator produced."
  (let ((base (or (cdr (assoc designator *library-basenames*))
                  (string-downcase (princ-to-string designator)))))
    (list #+darwin (format nil "lib~A.dylib" base)
          #+(and unix (not darwin)) (format nil "lib~A.so" base)
          #+windows (format nil "~A.dll" base))))

(defun %load-one (designator &key (errorp t))
  "Open DESIGNATOR, recording where it came from. Returns the namestring or NIL.

When ERRORP is false a failure to open is reported as NIL rather than
signalled, which is what the optional add-ons want."
  (let ((cffi:*foreign-library-directories*
          (append (%search-directories) cffi:*foreign-library-directories*)))
    (handler-case
        (let* ((handle (cffi:load-foreign-library designator))
               (path (or (ignore-errors
                          (let ((p (cffi:foreign-library-pathname handle)))
                            (when p (namestring p))))
                         (string-downcase (princ-to-string designator)))))
          (setf (gethash designator *loaded*) path)
          path)
      (cffi:load-foreign-library-error (e)
        (remhash designator *loaded*)
        (when errorp
          (cl:error 'library-not-found
                    :detail (string-downcase (princ-to-string designator))
                    :candidates
                    (append (mapcar (lambda (d)
                                      (format nil "~A (directory searched)" d))
                                    (%search-directories))
                            (%candidate-names designator)
                            (list (princ-to-string e)))))
        nil))))

(defun %load-process-library ()
  "Open exactly one of im_process / im_process_omp, honouring *PROCESS-LIBRARY*.

Only one, because their symbol sets are identical: with both open, which
implementation a call reaches is decided by dlsym's search order rather than
by anything this code says."
  (ecase *process-library*
    ;; An explicit choice is a requirement: asking for OpenMP and silently
    ;; getting the serial build would make a performance measurement a lie.
    (:openmp (%load-one 'lib-im-process-omp))
    (:serial (%load-one 'lib-im-process))
    ;; :AUTO is a preference, and neither being present is a supported
    ;; configuration -- upstream's IM_BUILD_PROCESS can be switched off, and a
    ;; caller who only reads and writes files needs none of it. Failing to load
    ;; the whole binding over a missing add-on would be the wrong trade.
    (:auto (or (%load-one 'lib-im-process-omp :errorp nil)
               (%load-one 'lib-im-process :errorp nil)))))

(defun load-libraries ()
  "Open libim, the processing library, and whichever add-ons are present.

Safe to call again; it closes nothing, and CFFI treats a second open of the
same library as a no-op."
  (dolist (d *core-libraries*)
    (%load-one d))
  (%load-process-library)
  (dolist (d *optional-libraries*)
    (%load-one d :errorp nil))
  (register-addon-formats)
  (loaded-libraries))

(defparameter *addon-format-registrars*
  '((lib-im-jp2  . "imFormatRegisterJP2")
    (lib-im-heif . "imFormatRegisterHEIF")
    (lib-im-avi  . "imFormatRegisterAVI")
    (lib-im-wmv  . "imFormatRegisterWMV"))
  "Format drivers that must announce themselves to IM's registry.

The built-in fourteen are registered by libim itself on first use. An add-on
cannot be: it is a separate shared object, and until its registrar runs IM has
never heard of it, so imFormatList omits it and opening a .heic reports an
unrecognised format. Calling these is what makes loading libim_heif mean
anything.

imFormatRegisterHEIF registers two drivers, HEIF and AVIF, from the one call.")

(defun register-addon-formats ()
  "Register the format drivers of whichever add-ons loaded. Idempotent.

IM tolerates a repeated registration -- it replaces the entry -- so this is
safe to run again after an image restore."
  (dolist (entry *addon-format-registrars*)
    (destructuring-bind (designator . symbol) entry
      (when (library-loaded-p designator)
        (let ((pointer (cffi:foreign-symbol-pointer symbol :library designator)))
          (when pointer
            (cffi:foreign-funcall-pointer pointer () :void)))))))

;;; Choosing between colliding FFT implementations ----------------------------

(defun fft-symbol (name)
  "A pointer to NAME in the FFT implementation that should be used.

libim_fftw3 exports six functions -- imProcessFFT, imProcessIFFT,
imProcessFFTraw, imProcessSwapQuadrants, imProcessAutoCorrelation and
imProcessCrossCorrelation -- under names libim_process already defines. Both
end up in the process, because libim_fftw3 links against libim_process, so a
plain foreign-funcall reaches whichever the dynamic linker happens to find
first. That is load-order dependent and differs between a source checkout and
a dumped image.

Resolving explicitly against a named library removes the ambiguity: if FFTW3
is present it wins, deliberately, because it is the faster and more accurate
implementation and the only reason to build the add-on."
  (or (when (library-loaded-p 'lib-im-fftw3)
        (cffi:foreign-symbol-pointer name :library 'lib-im-fftw3))
      (cffi:foreign-symbol-pointer name)
      (cl:error 'im-error
                :detail (format nil "~A is not available in any loaded IM library" name))))

;;; Image dump and restore ----------------------------------------------------
;;;
;;; A saved image comes back with no foreign libraries open, but with CFFI's
;;; bookkeeping saying otherwise. Both halves are needed: closing before the
;;; dump so the saved image carries no record of a library, and re-opening on
;;; restore so the new process binds the one it was told to.

(defun %prepare-for-dump ()
  (dolist (entry (loaded-libraries))
    (ignore-errors (cffi:close-foreign-library (car entry))))
  (clrhash *loaded*))

(defun %reinitialize ()
  ;; Close first. CFFI may still believe these are open from before the dump,
  ;; in which case LOAD-FOREIGN-LIBRARY would return without calling dlopen
  ;; and the process would keep whatever the loader gave it at startup.
  (dolist (d (append *core-libraries*
                     (list 'lib-im-process 'lib-im-process-omp)
                     *optional-libraries*))
    (ignore-errors (cffi:close-foreign-library d)))
  (clrhash *loaded*)
  ;; A binary that cannot find its libraries should say so on the first line
  ;; of output, not fail later inside an unrelated operation.
  (handler-case (load-libraries)
    (library-not-found (c)
      (format *error-output* "~&~A~%" c)
      (uiop:quit 1))))

(uiop:register-image-dump-hook '%prepare-for-dump)
(uiop:register-image-restore-hook '%reinitialize nil)

(load-libraries)
