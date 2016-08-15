#lang racket

(provide (all-defined-out))

(define (read-grader-mapping-file file)
  (with-input-from-file file
    (λ ()
      (define data (read))
      (validate-grader-mappings data)
      data)))

(define (validate-grader-mappings data)
  (unless (list? data)
    (error 'validate-grader-mappings "Ill formed grader-database: ~a" data))
  (for ([i (in-list data)])
    (unless (and (pair? i)
                 (string? (car i))
                 (string? (cdr i)))
      (error 'validate-grader-mappings "Ill formed grader database: ~a" i))))
  
(define (find-grader mapping student)
  (validate-grader-mappings mapping)
  (dict-ref mapping student))

