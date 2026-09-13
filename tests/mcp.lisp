;;;; tests/mcp.lisp — the MCP server's JSON-RPC dispatch, in process.
;;;;
;;;; These drive HANDLE-LINE directly with the standard output captured, so
;;;; they check the wire behaviour -- one JSON object per line, notifications
;;;; silent, errors well-formed -- without a subprocess or a socket.

(in-package #:im.tests)

(def-suite mcp-suite :in im-suite
  :description "The im/mcp Model Context Protocol server.")
(in-suite mcp-suite)

(defun mcp (json)
  "Feed one JSON-RPC line to the server; return its reply parsed, or NIL if it
wrote nothing (a notification)."
  (let ((out (with-output-to-string (s)
               (let ((*standard-output* s))
                 (im.mcp::handle-line json)))))
    (when (plusp (length (string-trim '(#\Space #\Newline) out)))
      ;; Also asserts the reply is a single line: a stray newline would make
      ;; this two messages to a real client.
      (is (= 1 (count #\Newline out)) "the reply is exactly one line")
      (shasht:read-json out))))

(defun mcp-call (name arguments-json)
  "Call tool NAME with a raw JSON arguments object; return the parsed reply."
  (mcp (format nil "{\"jsonrpc\":\"2.0\",\"id\":9,\"method\":\"tools/call\",~
                     \"params\":{\"name\":\"~A\",\"arguments\":~A}}"
               name arguments-json)))

(defun image-json (name)
  "A JSON string literal for a fixture's path."
  (format nil "\"~A\"" (namestring (image-file name))))

(test initialize-announces-the-protocol-and-tools-capability
  (let* ((reply (mcp "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\",\"params\":{}}"))
         (result (gethash "result" reply)))
    (is (equal "2.0" (gethash "jsonrpc" reply)))
    (is (equal "2024-11-05" (gethash "protocolVersion" result)))
    (let ((capabilities (gethash "capabilities" result)))
      (is-true (and (hash-table-p capabilities)
                    (nth-value 1 (gethash "tools" capabilities)))
               "advertises a tools capability"))
    (is (equal "im" (gethash "name" (gethash "serverInfo" result))))))

(test tools-list-describes-every-tool
  (let* ((reply (mcp "{\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"tools/list\"}"))
         (tools (gethash "tools" (gethash "result" reply)))
         (names (map 'list (lambda (h) (gethash "name" h)) tools)))
    (dolist (want '("im_info" "im_stats" "im_diff" "im_thumbnail" "im_montage" "im_formats"
                    "im_analyze" "im_process"))
      (is (member want names :test #'string=) "~A is advertised" want))
    ;; every tool carries a JSON-Schema object with a type
    (is (every (lambda (h) (equal "object" (gethash "type" (gethash "inputSchema" h)))) tools))))

(test a-notification-draws-no-reply
  "A message with no id is a notification; JSON-RPC forbids answering it."
  (is (null (mcp "{\"jsonrpc\":\"2.0\",\"method\":\"notifications/initialized\"}"))))

(test an-unknown-method-is-a-json-rpc-error
  (let ((reply (mcp "{\"jsonrpc\":\"2.0\",\"id\":3,\"method\":\"no/such\"}")))
    (is (= -32601 (gethash "code" (gethash "error" reply))))))

(test a-malformed-line-is-a-parse-error-not-a-crash
  (let ((reply (mcp "{ this is not json")))
    (is (= -32700 (gethash "code" (gethash "error" reply))))))

(test im-info-tool-returns-the-format-as-text
  (let* ((reply (mcp-call "im_info" (format nil "{\"path\":~A}" (image-json "lena.jpg"))))
         (content (gethash "content" (gethash "result" reply))))
    (is (equal "text" (gethash "type" (aref content 0))))
    (is (search "JPEG" (gethash "text" (aref content 0))))))

(test im-diff-tool-reports-identical-images
  (let* ((path (image-json "lena.jpg"))
         (reply (mcp-call "im_diff" (format nil "{\"first\":~A,\"second\":~A}" path path)))
         (text (gethash "text" (aref (gethash "content" (gethash "result" reply)) 0))))
    (is (search "identical" text))))

(test im-thumbnail-tool-returns-an-inline-png
  "The point of the server: an agent that asks for a thumbnail gets the image,
not a path it cannot open."
  (let* ((reply (mcp-call "im_thumbnail"
                          (format nil "{\"path\":~A,\"max\":48}" (image-json "lena.jpg"))))
         (content (gethash "content" (gethash "result" reply)))
         (image (find "image" content :key (lambda (h) (gethash "type" h)) :test #'string=)))
    (is-true image "there is image content")
    (is (equal "image/png" (gethash "mimeType" image)))
    ;; The PNG's fixed 8-byte signature base64-encodes to this constant prefix,
    ;; so a real PNG payload always starts with it -- no decoder needed.
    (is (eql 0 (search "iVBORw0KGgo" (gethash "data" image)))
        "the data is base64 of a real PNG")))

(test a-tool-error-is-a-result-not-a-protocol-error
  "A tool that fails reports isError in its result; the call itself succeeded."
  (let* ((reply (mcp-call "im_info" "{\"path\":\"/no/such/file.png\"}"))
         (result (gethash "result" reply)))
    (is (null (gethash "error" reply)) "not a JSON-RPC error")
    (is (eq t (gethash "isError" result)) "the result is flagged as an error")))

;;; The processing tools -------------------------------------------------------

(defun mcp-text (reply)
  "The text content of a tools/call reply, concatenated."
  (let ((content (gethash "content" (gethash "result" reply))))
    (with-output-to-string (out)
      (loop for item across content
            when (equal "text" (gethash "type" item))
              do (write-string (gethash "text" item) out)))))

(defun mcp-error-p (reply)
  (gethash "isError" (gethash "result" reply)))

(test im-analyze-tool-counts-and-measures-regions
  "The question a thumbnail cannot answer: how many objects, and how big."
  (let* ((reply (mcp-call "im_analyze"
                          (format nil "{\"path\":~A,\"measure\":\"all\",\"limit\":1}"
                                  (image-json "rice.png"))))
         (report (shasht:read-json (mcp-text reply))))
    (is (not (mcp-error-p reply)))
    (is (> (gethash "region-count" report) 10)
        "rice.png carries dozens of grains")
    (is (equal "connected-components" (gethash "method" report)))
    (let ((region (aref (gethash "regions" report) 0)))
      ;; One field per measurement family, so a silently dropped family fails.
      (dolist (field '("area" "x" "xmin" "hull-area" "max-feret" "intensity-mean"))
        (is-true (nth-value 1 (gethash field region))
                 "--measure all must report ~A" field)))))

(test im-analyze-tool-splits-touching-objects-on-request
  "watershed is the reason this tool is worth having over `count the blobs'.

Touching grains are one connected region each and several objects, so the
count must go UP and the reported method must say which produced it."
  (let* ((plain (shasht:read-json
                 (mcp-text (mcp-call "im_analyze"
                                     (format nil "{\"path\":~A,\"limit\":0}"
                                             (image-json "rice.png"))))))
         (split (shasht:read-json
                 (mcp-text (mcp-call "im_analyze"
                                     (format nil "{\"path\":~A,\"limit\":0,\"watershed\":true}"
                                             (image-json "rice.png")))))))
    (is (equal "watershed" (gethash "method" split)))
    (is (> (gethash "region-count" split) (gethash "region-count" plain)))))

(test im-analyze-tool-reports-a-bad-measurement-as-a-tool-error
  "A wrong argument is an isError result naming the alternatives, not a crash
and not a protocol error -- the model is meant to read it and try again."
  (let ((reply (mcp-call "im_analyze"
                         (format nil "{\"path\":~A,\"measure\":\"nope\"}"
                                 (image-json "rice.png")))))
    (is-true (mcp-error-p reply))
    (is (search "unknown measurement" (mcp-text reply)))))

(test im-process-tool-runs-a-pipeline-and-returns-an-image
  "Including an operation added in this release, so the tool cannot pass
against a build that lacks it."
  (let* ((reply (mcp-call "im_process"
                          (format nil "{\"path\":~A,\"ops\":[\"threshold=otsu\",\"watershed\"]}"
                                  (image-json "rice.png"))))
         (content (gethash "content" (gethash "result" reply)))
         (image (find "image" content :key (lambda (h) (gethash "type" h)) :test #'equal))
         (report (shasht:read-json (mcp-text reply))))
    (is (not (mcp-error-p reply)))
    (is-true image "the result carries an inline image")
    (is (equal "image/png" (gethash "mimeType" image)))
    (is (plusp (length (gethash "data" image))))
    ;; A watershed writes a label image, so the pipeline really ran.
    (is (equal "data-type-ushort" (gethash "data-type" report)))
    (is (= 256 (gethash "width" report)) "the reported size is the full one")))

(test im-process-tool-scales-the-preview-and-can-write-the-full-size-result
  "The inline image is a preview; `output' is how the full-size result escapes."
  (let* ((out (namestring (tmp-file "mcp-process.png")))
         (reply (mcp-call "im_process"
                          (format nil "{\"path\":~A,\"ops\":[\"gaussian=1.5\"],~
                                       \"preview\":64,\"output\":\"~A\"}"
                                  (image-json "rice.png") out)))
         (report (shasht:read-json (mcp-text reply))))
    (is (not (mcp-error-p reply)))
    (is (<= (gethash "preview-width" report) 64))
    (is (= 256 (gethash "width" report)) "the preview must not shrink the real result")
    (is-true (probe-file out) "output was written")
    (im:with-image (written (im:load (pathname out)))
      (is (= 256 (im:width written)) "the file is full size, not the preview"))))

(test im-process-tool-rejects-an-unknown-operation
  (let ((reply (mcp-call "im_process"
                         (format nil "{\"path\":~A,\"ops\":[\"nosuchop\"]}"
                                 (image-json "rice.png")))))
    (is-true (mcp-error-p reply))
    (is (search "unknown operation" (mcp-text reply)))))
