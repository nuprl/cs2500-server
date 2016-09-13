#lang racket/base

(provide current-pairs-file-list
         valid-pairing)

(require racket/function
         "../bin/utils/constants.rkt")

(define pairs-files
  '("pairs1.rktd"
    "pairs2.rktd"
    "pairs3.rktd"
    "pairs4.rktd"
    "pairs5.rktd"
    "pairs6.rktd"
    "pairs7.rktd"
    "pairs8.rktd"
    "pairs9.rktd"
    "pairs10.rktd"
    "pairs11.rktd"
    "pairs12.rktd"
    "pairs14.rktd"))

(define (error* fmt . args)
  (error (apply format fmt args)))

(define (get-pairs pairs-file)
  (with-input-from-file pairs-file
    (λ () (read))))

(define current-pairs-file-list
  (make-parameter
   (for/list ([i (in-list pairs-files)])
     (build-path student-server-dir i))))

;; usernames: (Listof String) or String
;; pair-file: file-path
;;
;; Errors if the student pair represented by usernames is not a listed pair in pair-file
(define (valid-pairing/file usernames pair-file)
  (define sorted-usernames (sort (if (list? usernames) usernames (list usernames))
                                 string<?))
  (for/fold ([acc #f])
            ([pair (get-pairs pair-file)])
    (or acc (equal? (sort pair string<?) sorted-usernames))))

(define (valid-pairing usernames)
  (unless (ormap (curry valid-pairing/file usernames) (current-pairs-file-list))
    (error* "Users not registered to work together: ~a" usernames)))
