#!/usr/bin/env newlisp

;; ln -s web.lsp index.cgi
;; newlisp -http -d 8000 -w .
;; curl -v http://localhost:8000/index.cgi?name=Ivan

(load {/usr/share/newlisp/modules/cgi.lsp})
(set 'file "/tmp/counter.txt")
(set 'content (int (or (read-file file) "0")))
(print "Content-type: application/json\r\n\r\n")
(set 'response (string "Hello " (CGI:get "name") "!"))
(println response)
(write-file file (string (inc content)))