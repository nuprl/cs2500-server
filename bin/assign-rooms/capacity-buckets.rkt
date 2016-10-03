#lang racket

;; ---------------------------------------------------------------------------------------------------
(provide
 (contract-out
  [allocate
   ;; allocate the given 'things' in lox over as many lists as capacities specified,
   ;; where each of these lists is at most as long as the respective capacity allows 
   (->i ([lox        list?]
         [capacities (listof natural-number/c)])
        #:pre/name (lox capacities)
        "do the given elements fit into the buckets with the given capacities"
        (<= (length lox) (apply + capacities))
        
        (buckets     (and/c list?))
        #:post/name (lox capacities buckets)
        "are there as many buckets as capacities & as many things in the buckets as elements"
        (and (= (length buckets) (length capacities))
             (= (length (apply append buckets)) (length lox))))]))

;; ---------------------------------------------------------------------------------------------------
(module+ test
  (require rackunit))

;; ---------------------------------------------------------------------------------------------------
;; the cool thing about allocate is that it actually shrinks the number of capacities as it goes
;; down the lox list BUT it is guaranteed to produce the correct number of buckets in the end
;; dang cool recursion

;; an alternative design would use a fixed-size vector of buckets and select only a bucket with
;; left-over capacity

(module+ test
  (define lox0 '(a b c d e f g))
  (define capacities0 '(3 2 2))
  (check-pred (lambda (x)
                (displayln x)
                (unless (= (length x) (length capacities0))
                  (error 'buckets "wrong number of buckets ~s" x))
                (unless (= (length (apply append x)) (length lox0))
                  (error 'result "wrong number of elements ~s ~s" x lox0))
                #true)
              (allocate lox0 capacities0)))

(define (allocate lox capacities)
  (cond
    ;; this first line is made superfluous by the contract 
    #;
    [(empty? capacities) (error "can't happen")]
    [(empty? lox) (make-list (length capacities) '())] ;; for contract only
    [else (define choice  (random (length capacities)))
          (define pick    (first lox))
          (define revised (revise-capacities choice capacities))
          (define buckets (allocate (rest lox) revised))
          (if (= (length revised) (length capacities))
              (up choice pick buckets)
              (add-new-bucket-at-proper-place choice pick buckets))]))

;; ---------------------------------------------------------------------------------------------------
;; N X [Listof [Listof X]] -> [Listof [Listof X]]
;; add x to the n-th bucket in distribution

(module+ test
  (check-equal? (up 0 'pick '(())) '((pick)))
  (check-equal? (up 1 'pick '((a) (b) (c))) '((a) (pick b) (c))))

(define (up choice pick distribution)
  (cond
    [(empty? distribution) (list (list pick))]
    [(zero? choice) (cons (if (empty? (first distribution))
                              (list pick)
                              (cons pick (first distribution)))
                          (rest distribution))]
    [else (cons (first distribution) (up (sub1 choice) pick (rest distribution)))]))

;; ---------------------------------------------------------------------------------------------------
;; N X [Listof [Listof X]] -> [Listof [Listof X]]
;; add x to the n-th bucket in distribution

(module+ test
  (check-equal? (add-new-bucket-at-proper-place 0 'pick '(())) '((pick) ()))
  (check-equal? (add-new-bucket-at-proper-place 1 'pick '((a) (b) (c))) '((a) (pick) (b) (c))))

(define (add-new-bucket-at-proper-place choice pick distribution)
  (cond
    [(zero? choice) (cons (list pick) distribution)]
    [else (cons (first distribution)
                (add-new-bucket-at-proper-place (sub1 choice) pick (rest distribution)))]))

;; ---------------------------------------------------------------------------------------------------
;; N [Listof N] -> [Listof N]
;; n is in the domain of capacities

(module+ test
  (check-equal? (revise-capacities 0 '(10 9 1)) '(9 9 1))
  (check-equal? (revise-capacities 0 '(1 9 1)) '(9 1))
  (check-equal? (revise-capacities 2 '(1 9 1)) '(1 9))
  (check-equal? (revise-capacities 1 '(1 9 1)) '(1 8 1)))

(define (revise-capacities n capacities)
  (cond
    [(zero? n)
     (if (= (first capacities) 1)
         (rest capacities)
         (cons (- (first capacities) 1) (rest capacities)))]
    [else (cons (first capacities) (revise-capacities (sub1 n) (rest capacities)))]))
