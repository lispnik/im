;;;; src/mcp/package.lisp — an MCP server exposing the toolkit to agents.
;;;;
;;;; Model Context Protocol over stdio: newline-delimited JSON-RPC 2.0 on
;;;; standard input and output. The server advertises a handful of tools --
;;;; inspect, compare, thumbnail, contact-sheet -- and, for the ones that
;;;; produce a picture, returns it inline as MCP image content, so an agent
;;;; asking for a thumbnail gets the thumbnail, not a path it cannot see.

(defpackage #:im.mcp
  (:use #:common-lisp)
  (:export #:main)
  (:documentation
   "A Model Context Protocol server for the IM binding.

Reuses the image algebra behind im(1) -- the perceptual hashes and SSIM of
`im diff', the compositing of `im montage' -- rather than reimplementing it,
so the two front ends cannot drift apart. Run it over stdio from an MCP
client; see MAIN."))
