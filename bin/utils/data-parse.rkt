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

(define (read-pairs pairs-path)
  (with-input-from-file pairs-path
    (λ ()
      (read))))

(define (read-users users-path)
  (define table
    (with-input-from-file users-path
      (λ ()
        (read))))
  (make-hash (for/list ([i (in-list table)])
               (cons (first i) (second i)))))

(define (read-config config-path)
  (define table
    (with-input-from-file config-path
      (λ ()
        (read))))
  (make-hash (for/list ([i (in-list table)])
               (cons (first i) (second i)))))

(define (write-config config-path table)
  (define out
    (for/list ([(k v) (in-dict table)])
      (list k v)))
  (with-output-to-file config-path #:exists 'replace
    (λ () (write out))))
  