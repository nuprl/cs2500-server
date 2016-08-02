#!/usr/bin/env racket
#lang racket

(require racket/runtime-path)

(define-runtime-path this-dir ".")
(define-runtime-path student-server (build-path ".." "student-server"))
(define-runtime-path grades-server (build-path ".." "grades-server"))

(void
 (system* (find-executable-path "openssl")
          "req" "-x509" "-newkey" "rsa:2048"
          "-keyout" "private-key.pem" "-out" "server-cert.pem" "-days" "365"))
 
(copy-file (build-path this-dir "private-key.pem") (build-path student-server "private-key.pem") #t)
(copy-file (build-path this-dir "private-key.pem") (build-path grades-server "private-key.pem") #t)
(copy-file (build-path this-dir "server-cert.pem") (build-path student-server "server-cert.pem") #t)
(copy-file (build-path this-dir "server-cert.pem") (build-path grades-server "server-cert.pem") #t)
(displayln "Remember to copy *only* server-cert.pem to the clients and update them")
