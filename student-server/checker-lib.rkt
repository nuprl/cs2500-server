#lang racket/base

(provide current-pairs-file-list
         valid-pairing)

(define pairs-files
  '("pairs1.rktd"
    "pairs2.rktd"))

(define (error* fmt . args)
  (error (apply format fmt args)))

(define (get-pairs pairs-file)
  (with-input-from-file pairs-file
    (λ () (read))))

(define current-pairs-file-list
  (make-parameter
   (for/list ([i (in-list pairs-files)])
     (build-path (current-directory) i))))

(define (valid-pairing/file usernames pair-file)
  (define sorted-usernames (if (list? usernames) usernames (list usernames)))
  (unless (member usernames (get-pairs pair-file))
    (if (list? usernames)
        (error* "Users not registered to work together: ~a" usernames)
        (error* "User ~a is not registered to work alone" usernames))))

(define (valid-pairing usernames)
  (map valid-pairing/file (current-pairs-file-list)))
