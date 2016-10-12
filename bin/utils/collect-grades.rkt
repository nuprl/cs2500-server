#lang racket

(provide (contract-out [build-gradebook (-> (dict-implements/c string? any/c)
                                            (dict-implements/c
                                             string? (dict-implements/c string? number?)))]))
(require "constants.rkt"
         "parse-comments.rkt")
(module+ test
  (require rackunit))


;; Collect all of the grades for a student
;; (Dictof Assitnments/String Part/Number ->
;;   (Dictof Student/String (Dictof Assignment#/String Grade/Number)
(define (build-gradebook assignments)
  (define gradebook (make-hash))
  (parameterize ([current-directory student-server-dir])
    (for ([(ass part) (in-dict assignments)])
      (unless (directory-exists? ass)
        (error 'gradebook "Assignment ~a does not exist!" ass))
      (for ([pair (in-directory ass)])
        (define-values (s1 s2) (split-students pair))
        (define file-to-grade (build-path ass pair (format graded-part-string part)))
        (define-values (grade total)
          (if (file-exists? file-to-grade)
              (get-point-values file-to-grade)
              (values 0 #f)))
        (when s1 (update-gradebook! gradebook s1 ass grade))
        (when s2 (update-gradebook! gradebook s2 ass grade)))))
  gradebook)

; (Dctorf Student (Dictof Assignment Grade)) String String Number -> Void
(define (update-gradebook! gradebook student assignment grade)
  (dict-update! gradebook student
                (λ (v)
                  (dict-set! v assignment grade)
                  v)
                (make-hash)))
(module+ test
  (check-equal? (let ([gb (make-hash)])
                  (update-gradebook! gb "bob" "43" "897")
                  gb)
                (make-hash `(("bob" . ,(make-hash '(("43" . "897"))))))))
