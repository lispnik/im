;;;; src/mcp/server.lisp — the JSON-RPC/stdio transport and dispatch.

(in-package #:im.mcp)

(defparameter *protocol-version* "2024-11-05"
  "The MCP revision this server speaks. A client that wants another gets this
one back in the initialize result and decides for itself.")

(defparameter *server-version*
  (or (ignore-errors (asdf:component-version (asdf:find-system "im/mcp"))) "unknown"))

;;; JSON value construction ----------------------------------------------------
;;;
;;; shasht maps NIL to false and :NULL to null, serialises a hash-table as an
;;; object and a vector as an array, and spells an empty object :EMPTY-OBJECT.
;;; OBJ builds the string-keyed hash-tables the protocol is made of.

(defun obj (&rest pairs)
  "A JSON object from alternating string keys and values."
  (loop with table = (make-hash-table :test #'equal)
        for (key value) on pairs by #'cddr
        do (setf (gethash key table) value)
        finally (return table)))

(defun write-message (message)
  "Serialise MESSAGE as one line of JSON on stdout.

shasht pretty-prints unconditionally, but a newline is legal JSON only inside
a string, where it is escaped -- so the structural newlines it inserts can be
stripped to satisfy MCP's one-message-per-line framing without touching any
value. The remaining indent spaces are legal inter-token whitespace."
  (let ((json (with-output-to-string (s) (shasht:write-json message s))))
    (write-string (remove #\Newline json) *standard-output*)
    (write-char #\Newline *standard-output*)
    (finish-output *standard-output*)))

(defun rpc-result (id result) (obj "jsonrpc" "2.0" "id" id "result" result))
(defun rpc-error (id code message)
  (obj "jsonrpc" "2.0" "id" id "error" (obj "code" code "message" message)))

(define-condition method-not-found (error)
  ((name :initarg :name :reader method-not-found-name)))

;;; Tool registry -------------------------------------------------------------
;;;
;;; The transport owns the registry; the tools themselves (and the image
;;; helpers they need) live in tools.lisp, which loads next and fills it.

(defstruct (tool (:constructor make-tool (name description schema handler)))
  name description schema handler)

(defvar *tools* nil "Registered tools, newest first; DISPATCH reverses for display.")

(defmacro define-tool (name description schema (args) &body body)
  "Register a tool. BODY runs with ARGS bound to the call's argument
hash-table and returns a list of content items."
  `(push (make-tool ,name ,description ,schema
                    (lambda (,args) (declare (ignorable ,args)) ,@body))
         *tools*))

(defun tool-descriptor (tool)
  (obj "name" (tool-name tool)
       "description" (tool-description tool)
       "inputSchema" (tool-schema tool)))

(defun arg (args name &optional default)
  (multiple-value-bind (value present) (gethash name args)
    (if present value default)))
(defun required-arg (args name)
  (multiple-value-bind (value present) (gethash name args)
    (unless present (error "missing required argument: ~A" name))
    value))

;;; Content and JSON-Schema shorthand ------------------------------------------

(defun text-content (control &rest args)
  (obj "type" "text" "text" (apply #'format nil control args)))
(defun image-content (base64)
  (obj "type" "image" "data" base64 "mimeType" "image/png"))

(defun schema (properties &key required)
  "A JSON-Schema object node. PROPERTIES is a plist of name and spec."
  (obj "type" "object"
       "properties" (loop with table = (make-hash-table :test #'equal)
                          for (name spec) on properties by #'cddr
                          do (setf (gethash name table) spec)
                          finally (return table))
       "required" (coerce required 'vector)))
(defun prop (type description) (obj "type" type "description" description))

(defun call-tool (name args)
  "Run the named tool and wrap the outcome as a tools/call result.

A tool that signals becomes a result with isError true, not a JSON-RPC error:
the call reached the tool, the tool reported the problem, and the model is
meant to see the message and adjust."
  (let ((tool (find name *tools* :key #'tool-name :test #'equal)))
    (handler-case
        (if tool
            (obj "content" (coerce (funcall (tool-handler tool) args) 'vector))
            (error "no such tool: ~A" name))
      (error (c)
        (obj "content" (vector (text-content "~A" c)) "isError" t)))))

;;; Dispatch -------------------------------------------------------------------

(defun dispatch (method params)
  "Handle one request METHOD with PARAMS (a hash-table), returning its result.

Signals METHOD-NOT-FOUND for anything unrecognised; TOOLS/CALL turns a failing
tool into an error *result* rather than a protocol error, which is what MCP
asks for -- the call succeeded, the tool reported trouble."
  (cond
    ((string= method "initialize")
     (obj "protocolVersion" *protocol-version*
          "capabilities" (obj "tools" :empty-object)
          "serverInfo" (obj "name" "im" "version" *server-version*)))
    ((string= method "ping") :empty-object)
    ((string= method "tools/list")
     (obj "tools" (coerce (mapcar #'tool-descriptor (reverse *tools*)) 'vector)))
    ((string= method "tools/call")
     (call-tool (gethash "name" params)
                (let ((a (gethash "arguments" params)))
                  (if (hash-table-p a) a (make-hash-table :test #'equal)))))
    (t (error 'method-not-found :name method))))

(defun handle-line (line)
  "Parse and act on one JSON-RPC message. Requests get a response; a
notification -- a message with no id -- gets none, per JSON-RPC."
  (let ((message (handler-case (shasht:read-json line)
                   (error ()
                     (write-message (rpc-error :null -32700 "parse error"))
                     (return-from handle-line)))))
    (unless (hash-table-p message) (return-from handle-line))
    (let ((id (gethash "id" message))
          (method (gethash "method" message))
          (params (gethash "params" message)))
      (when (null method) (return-from handle-line))
      (let ((params (if (hash-table-p params) params (make-hash-table :test #'equal))))
        (handler-case
            (let ((result (dispatch method params)))
              (when id (write-message (rpc-result id result))))
          (method-not-found (c)
            (when id (write-message (rpc-error id -32601
                                               (format nil "method not found: ~A"
                                                       (method-not-found-name c))))))
          (error (c)
            (when id (write-message (rpc-error id -32603 (princ-to-string c))))))))))

(defun main ()
  "Serve MCP over stdin/stdout until end of input.

Newline-delimited: one JSON-RPC message per line. Blank lines are ignored, and
a message that fails to parse gets a JSON-RPC parse error rather than taking
the server down -- a long-lived server has to survive a bad line."
  (loop for line = (read-line *standard-input* nil :eof)
        until (eq line :eof)
        do (unless (zerop (length (string-trim '(#\Space #\Tab #\Return) line)))
             (handle-line line))))
