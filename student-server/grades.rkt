#lang racket/base

(define grades-conf
  ;; e.g. hw1 max is 44 , hw2 max is 50, exam3 max is 26
  ;; Do not include bonus points, otherwise they won't be counted as bonus
  ;; tag   Display-Name Weight Max-Points
 ;`([hw    "Homework"   30     (70 133 159 80 87 72 64 61 64 94 98 143)]
  '([hw    "Homework"   30     (30 36)]
    [honors-hw    "Honors Homework"   30     (30 36)]
    [exam  "Exam"       55     (60 55)]
    [quiz  "Quiz"       10      (1 1 1 1 1 1 1 1 1 1)]
    [whim  "Whim"       5      (5)]))


(define name-format "Student: {username} Lab: {Lab} Grader: {Grader}")

(define max-score 100)

(define smooth-range '(8 6))

(define graded-file "graded.rkt")
(define orig-file   "grading/text.rkt")
(define grade-file  "grade")
(define html-file   "graded.html")

(define honors-labs '("lab7"))
(define honors-homeworks
  '("honors-exam1" "honors-hw3" "honors-quiz1" "honors-quiz2" "honors-hw5" "honors-quiz3" "honors-quiz4" "honors-quiz5"))
(define regular-homeworks
  '("hw3" "quiz1" "quiz2" "hw5" "quiz3" "quiz4"))

;; ----------------------------------------------------------------------------

(require racket/list racket/match racket/file)

(provide name-format smooth-range graded-file orig-file grade-file html-file)

(define-struct conf (tag name tag-total weight) #:prefab)

(define hwname->conf
  (let ([t (make-hash)])
    (define (hwname->conf hwname)
      (define m (regexp-match #rx"^([a-zA-Z-]+)([0-9]+)?$" hwname))
      (define conf (and m (assq (string->symbol (cadr m)) grades-conf)))
      (define num (and m (and (caddr m) (string->number (caddr m)))))
      (define weights (and conf (last conf)))
      (and weights (<= 1 (or num 1) (length weights))
           (let ([w (list-ref weights (sub1 (or num 1)))])
             (make-conf (car conf)
                        (if num (format "~a #~a" (cadr conf) num) (cadr conf))
                        (caddr conf) w))))
    (λ (hwname) (hash-ref! t hwname (λ () (hwname->conf hwname))))))

(provide hw-to-grade? hw-title hw-max-score)
(define (hw-to-grade? hwname) (and (hwname->conf hwname) #t))
(define (hw-title     hwname) (conf-name (hwname->conf hwname)))
(define (hw-max-score hwname) (conf-weight (hwname->conf hwname)))

(provide grade-description)
(define (grade-description grade)
  (if (number? grade)
    #f
    (case grade
      [(- --) "no submission"]
      [(? ??) "not graded yet"]
      [(* **) "waived"]
      [(*-) "optional hw waived"]
      [else (error 'grade-description "bad grade value: ~s" grade)])))

;; returns a number or #f to ignore it
(define (grade->number grade)
  (if (number? grade)
    grade
    (case grade
      [(* ** *- ? ??) #f]
      [(- --) 0]
      [else (error 'grade->number "bad grade value: ~s" grade)])))

(provide student-single-grade student-total-grade additional-filter+sort)
(define (weighted grades+weights)
  (and (pair? grades+weights)
       (let ([grades  (map car grades+weights)]
             [weights (map cdr grades+weights)])
         (/ (apply + (map * grades weights)) (apply + weights)))))
(define tag-names   (map car grades-conf))
(define tag-weights (map caddr grades-conf))
(define (student-total-grade hws+grades student info)
  (define hws    (map car hws+grades))
  (define grades (map cdr hws+grades))
  (define confs  (map hwname->conf hws))
  (define tag-grades+weights
    (for/list ([tag tag-names] [tag-weight tag-weights])
      (define tag-grade
        (weighted (filter-map
                   (λ (conf grade)
                     (and (eq? tag (conf-tag conf))
                          (let ([grade (grade->number grade)])
                            (and grade (cons grade (conf-weight conf))))))
                   confs grades)))
      (and tag-grade (cons tag-grade tag-weight))))
  (weighted (filter values tag-grades+weights)))
(define master-homeworks
  (get-preference 'pl-course:master-homeworks (λ () '())))
(define (student-single-grade hwname grade student info)
  (match grade
    [(list n m) (/ n m)]
    [(? number?) (/ grade max-score)]
    [(? symbol?) grade]
    [#f '--
       (cond
         [(member (info "Lab") honors-labs)
           (if (member hwname regular-homeworks) '*- '--)]
         [(not (member (info "Lab") honors-labs))
           (if (member hwname honors-homeworks) '*- '--)])]
    #;[#f '-- (if (and (member hwname master-homeworks) (equal? "U" (info "Status")))
          '*- '--)]
    [_ (error 'student-single-grade
              "bad grade for ~s/~a: ~s" student hwname grade)]))

(define additional-filter+sort
  (let ([filtered
         (λ (name status)
           (λ (xs)
             (cons name (filter (λ (x) (equal? status (x "Status"))) xs))))]
        [global-key
         (λ (x) (x '(substs "{Status} {FormalLast} {FormalFirst}")))])
    (list (filtered "Undergrads" "U")
          (filtered "Masters" "M")
          (λ (xs) (cons "Sorted" (sort xs string<?
                                       #:key global-key #:cache-keys? #t))))))
