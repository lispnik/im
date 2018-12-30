(in-package #:im)

(export '(im-error
	  error-code
	  message))

(define-condition im-error (error)
  ((error-code :initarg :error-code
	       :reader error-code)
   (message :initarg :message
	    :type string
	    :reader message))
  (:report (lambda (condition stream)
	     (format stream "IM error with code ~A: ~A"
		     (error-code condition)
		     (message condition)))))

(defvar *error-code-messages*
  `((:error-code-none . "No error")
    (:error-code-open . "Error while opening the file (read or write)")
    (:error-code-access . "Error while accessing the file (read or write)")
    (:error-code-format . "Invalid or unrecognized file format")
    (:error-code-data . "Invalid or unsupported data")
    (:error-code-compress . "Invalid or unsupported compression")
    (:error-code-mem . "Insufficient memory")
    (:error-code-counter . "Interrupted by the counter")))

(defun im-error-from-code (error-code)
  (let ((error-message (assoc-value *error-code-messages* error-code)))
    (if error-message
	(make-instance 'im-error
		       :error-code error-code
		       :message error-message)
	(error "Unknown error code ~A" error-code))))

(defun maybe-error (error-code)
  (unless (eq :error-code-none error-code)
    (error (im-error-from-code error-code))))

