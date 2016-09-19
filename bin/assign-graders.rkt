#!/usr/bin/env racket
#lang racket

;; Usage: ./assign-graders.rkt
;;
;; Writes a new grader-mapping.rkt in student server that assigns graders to pairs in a round-robin
;; fashion.

(require "utils/constants.rkt"
         "utils/data-parse.rkt")

(define graders (hash-keys (read-users (build-path grader-server-dir "users.rktd"))))
(define pairs (read-pairs (build-path student-server-dir "pairs.rktd")))

(define-values (mappings index)
  (for/fold ([mappings null]
             [grader-index 0])
            ([pair pairs])
    (define pair-name (string-join (sort pair string<?) "+"))
    (values
     (cons (cons pair-name (symbol->string (list-ref graders grader-index))) mappings)
     (remainder (add1 grader-index) (length graders)))))

(write-grader-mapping mappings (build-path student-server-dir graders-mapping-file))
