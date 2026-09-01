;;;; tests/workbench.lisp — the REPL toolkit: PIPE, DERIVE, the wrappers, SHOW.
;;;;
;;;; The auto-render hook (ENABLE-REPL-IMAGES) needs a live SLIME on the other
;;;; end and so is not exercised here beyond its refusal in a bare Lisp; the
;;;; rest is plain image algebra and is tested fully.

(in-package #:im.tests)

(def-suite workbench-suite :in im-suite
  :description "PIPE and the functional operation wrappers.")
(in-suite workbench-suite)

(test derive-returns-a-fresh-same-shape-image
  "DERIVE hands back the destination it allocated, the source's shape, and
leaves the source alone."
  (im:with-image (src (gray-gradient 16 16))
    (let ((result (im:derive src #'im:negative)))
      (unwind-protect
           (progn
             (is (im:imagep result))
             (is (not (eq src result)))
             (is (= 16 (im:width result)))
             (is (= 16 (im:height result)))
             (is (eq (im:data-type src) (im:data-type result)))
             ;; source survives; negative of a gradient is not the gradient
             (is (not (im:destroyed-p src)))
             (is (/= (pixel src 0 1 0) (pixel result 0 1 0))))
        (im:destroy result)))))

(test grayscale-produces-grey-and-is-always-fresh
  (im:with-image (rgb (im:create 8 8 :color-space-rgb :data-type-byte))
    (im:clear rgb)
    (let ((g (im:grayscale rgb)))
      (unwind-protect (is (eq :color-space-gray (im:color-space g)))
        (im:destroy g))))
  ;; already grey: a distinct copy, not the same object -- so PIPE can reclaim
  ;; it without touching the caller's image.
  (im:with-image (gray (gray-gradient 8 8))
    (let ((g (im:grayscale gray)))
      (unwind-protect
           (progn (is (not (eq gray g)))
                  (is (eq :color-space-gray (im:color-space g))))
        (im:destroy g)))))

(test resized-scales-and-keeps-aspect
  (im:with-image (src (im:create 200 100 :color-space-rgb :data-type-byte))
    ;; explicit scale
    (let ((half (im:resized src :scale 0.5)))
      (unwind-protect (progn (is (= 100 (im:width half)))
                             (is (= 50 (im:height half))))
        (im:destroy half)))
    ;; width only -> height follows the aspect ratio
    (let ((w (im:resized src :width 100)))
      (unwind-protect (progn (is (= 100 (im:width w)))
                             (is (= 50 (im:height w))))
        (im:destroy w)))))

(test pipe-threads-left-to-right-and-returns-the-final-image
  (im:with-image (source (im:load (image-file "lena.jpg")))
    ;; 208x222 -> half -> grey -> sobel. The result is the final stage's image.
    (let ((result (im:pipe source
                           (im:resized :scale 0.5)
                           #'im:grayscale
                           (im:derive #'im:convolve-sobel))))
      (unwind-protect
           (progn
             (is (= 104 (im:width result)))
             (is (= 111 (im:height result)))
             (is (eq :color-space-gray (im:color-space result))))
        (im:destroy result)))))

(test pipe-leaves-its-input-alive
  "The image handed to PIPE is the caller's; PIPE reclaims intermediates but
must not free the input or the returned result."
  (im:with-image (source (gray-gradient 32 32))
    (let ((result (im:pipe source
                           (im:derive #'im:negative)
                           (im:derive #'im:negative))))
      (unwind-protect
           (progn
             (is (not (im:destroyed-p source)) "PIPE destroyed its own input")
             (is (not (im:destroyed-p result)) "PIPE destroyed the result it returned")
             (is (not (eq source result))))
        (im:destroy result)))
    ;; the input is still usable afterwards
    (is (= 32 (im:width source)))))

(test pipe-tolerates-a-stage-that-returns-its-argument
  "An in-place stage that returns the image it was given must not be freed out
from under the next stage."
  (im:with-image (source (gray-gradient 16 16))
    (flet ((in-place (image) (im:clear image) image))  ; mutate, return same
      (let ((result (im:pipe source
                             #'in-place
                             (im:derive #'im:negative))))
        (unwind-protect
             (progn (is (not (im:destroyed-p source)))
                    (is (im:imagep result)))
          (im:destroy result))))))

(test show-summarises-displays-and-returns-the-image
  (let (shown)
    (let ((im:*display-function* (lambda (image pathname)
                                   (declare (ignore image pathname))
                                   (setf shown t)
                                   :test-harness)))
      (im:with-image (image (gray-gradient 8 8))
        (let ((summary (with-output-to-string (out)
                         (let ((returned (im:show image :stream out)))
                           (is (eq image returned) "SHOW returns its argument")))))
          (is-true shown "SHOW displayed the image")
          (is (search "8x8" summary) "the summary names the geometry")
          (is (search "plane 0" summary) "the summary lists per-plane stats"))))))

(test show-without-a-front-end-still-summarises
  "DISPLAY failing (no front end) must not stop SHOW returning its summary."
  (let ((im:*display-function* (lambda (image pathname)
                                 (declare (ignore image pathname))
                                 nil)))  ; declines -> DISPLAY signals, SHOW swallows
    (im:with-image (image (gray-gradient 4 4))
      (finishes (im:show image :stream (make-string-output-stream))))))

(test repl-image-rendering-refuses-cleanly-without-slime
  "In a bare Lisp (no swank-repl) ENABLE-REPL-IMAGES is an error, not a crash,
and DISABLE-REPL-IMAGES has nothing to undo."
  (unless (find-package '#:swank-repl)
    (signals im:im-error (im:enable-repl-images))
    (is (null (im:disable-repl-images)))))
