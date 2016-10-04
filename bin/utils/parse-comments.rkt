#lang racket

(provide (contract-out
          [grade-file (-> path? path? void?)]))
(require data/gvector
         wxme
         syntax-color/racket-lexer)

(module+ test
  (require rackunit))

;; Collects all of the comments in a Racket or BSL file
;; Path -> (GVectorof String)
(define (grab-comments file)
  (call-with-input-file* file
    (λ (p)
      (define comments (make-gvector))
      (define tp
        (if (is-wxme-stream? p)
            (wxme-port->text-port p)
            p))
      (let loop ()
        (match-define-values
         (text type paren? start-num end-num)
         (racket-lexer tp))
        (when (eq? type 'comment)
          (gvector-add! comments text))
        (unless (eof-object? text)
          (loop)))
      comments)))

(module+ test
  (check-equal?
   (grab-comments "../tests/bsl.rkt")
   (gvector
    "; The first three lines of this file were inserted by DrRacket. They record metadata"
    "; about the language level of this file in a form that our tools can easily process."
    ";> <+10>"
    ";> <-5> Why?"))
  (check-equal?
   (grab-comments "../bsl2.rkt")
   (gvector
    "; The first three lines of this file were inserted by DrRacket. They record metadata"
    "; about the language level of this file in a form that our tools can easily process."
    "; hello world")))

;; Remove all comments except for grader ones (begins with ;>)
;; (GVectorof String) -> (GVectorof String
(define (comments->grader-comments comments)
  (for/gvector ([s (in-gvector comments)]
                #:when (regexp-match #rx"^;>" s))
    s))

(module+ test
  (check-equal?
   (comments->grader-comments
    (gvector ";> hello" "world" ";> <+10> points"))
   (gvector ";> hello" ";> <+10> points")))

;; Grab comments for their grades stored as <-5>
;; (GVectorof String) -> (GVectorof Number)
(define grade-regexp #rx"<([+-][0-9]+)>")
(define (grab-grades comments)
  (for/gvector ([s (in-gvector comments)]
            #:when (regexp-match grade-regexp s))
    (define grades
      (regexp-match* grade-regexp s
                     #:match-select second))
    (apply values
           (map string->number grades))))

(module+ test
  (check-equal?
   (grab-grades
    (gvector "<+10>" "<-5>" "NO" "<+1> <-2>"))
   (gvector 10 -5 1 -2)))

;; Path Path -> Void
(define (grade-file file grade-path)
  (define grade
    (let* ([acc file]
           [acc (grab-comments acc)]
           [acc (comments->grader-comments acc)]
           [acc (grab-grades acc)]
           [acc (gvector->list acc)]
           [acc (apply + 0 acc)])
      acc))
  (with-output-to-file grade-path
    #:exists 'replace
    (λ ()
      (printf "~a" grade))))

(module+ test
  (grade-file "tests/bsl.rkt" "foo"))
