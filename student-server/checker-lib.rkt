#lang racket/base

(provide current-pairs-file
         valid-pairing)

(define (error* fmt . args)
  (error (apply format fmt args)))

(define (get-pairs pairs-file)
  (with-input-from-file pairs-file
    (λ () (read))))

(define current-pairs-file (make-parameter (build-path (current-directory) "pairs.rktd")))

(define (valid-pairing usernames)
  (define sorted-usernames (if (list? usernames) usernames (list usernames)))
  (unless (member usernames (get-pairs (current-pairs-file)))
    (if (list? usernames)
        (error* "Users not registered to work together: ~a" usernames)
        (error* "User ~a is not registered to work alone" usernames))))
