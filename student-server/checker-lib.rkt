#lang racket/base

(provide current-pairs-file-list
         valid-pairing)

(require racket/function)

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

;; usernames: (Listof String) or String
;; pair-file: file-path
;;
;; Errors if the student pair represented by usernames is not a listed pair in pair-file
(define (valid-pairing/file usernames pair-file)
  (define sorted-usernames (if (list? usernames) usernames (list usernames)))
  (unless (member usernames (get-pairs pair-file))
    (if (list? usernames)
        (error* "Users not registered to work together: ~a" usernames)
        (error* "User ~a is not registered to work alone" usernames))))

(define (valid-pairing usernames)
  (map (curry valid-pairing/file usernames) (current-pairs-file-list)))
