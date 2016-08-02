#!/usr/bin/env racket
#lang racket

(require racket/runtime-path)

(define-runtime-path this-dir ".")
(define-runtime-path student-server (build-path ".." "student-server"))
(define-runtime-path grades-server (build-path ".." "grades-server"))

(system* (find-executable-path "openssl")
         "req" "-x509" "-newkey" "rsa:2048"
         "-keyout" "private-key.pem" "-out" "server-cert.pem" "-days" "365")

(copy-file (build-path this-dir "private-key.pem") student-server)
(copy-file (build-path this-dir "private-key.pem") grades-server)
(copy-file (build-path this-dir "server-cert.pem") student-server)
(copy-file (build-path this-dir "server-cert.pem") grades-server)
(displayln "Remember to copy *only* server-cert.pem to the clients and update them")
