#!/usr/bin/env racket
#lang racket

;; Usage: ./gen-users.rkt raw-user-list [existing-users-file]
;;
;; Reads in a file of the form (listof (fullname email)) (given as the first command-line argument)
;; and writes to stdout a new user.rktd file with a new user entry for each entry in the input. If an
;; existing users file is given, those users are prepended to the output.

;; Generates a (plaintext) password consisting of a lowercase letter followed by up to 6 digits
(define (gen-password)
  (string-append (string (integer->char (+ 97 (random 26)))) (number->string (random 1000000))))

;; gen-users
;; (listof (fullname email)) -> void
;; given a list of grader names and email addresses, generate an initial
;; users.rtkd. Every user has a default password of "password"
(define (gen-users existing-users ls)
  (pretty-write
   (append
    existing-users
    (for/list ([p ls])
      (let* ([pass (gen-password)]
             [name (car p)]
             [email (cadr p)]
             [user (string->symbol (car (string-split email "@")))])
        `(,user ((plaintext ,pass) ,name ,email "" "")))))))

(define existing-users
  (if (> (vector-length (current-command-line-arguments)) 1)
      (with-input-from-file (vector-ref (current-command-line-arguments) 1) read)
      null))

(gen-users existing-users
           (with-input-from-file (vector-ref
                                   (current-command-line-arguments) 0) read))
