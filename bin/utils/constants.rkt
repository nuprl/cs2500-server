#lang racket

(provide (all-defined-out))

(require racket/runtime-path
         glob)
(module+ test
  (require rackunit))

;; These constants assume the github structure has not changed
;;   adjust them if needed
(define-runtime-path utils-dir ".")
(define base-dir (build-path utils-dir ".." ".."))
(define student-server-dir (build-path base-dir "student-server"))
(define student-config-file (build-path student-server-dir "config.rktd"))
(define grader-server-dir (build-path base-dir "grades-server"))
(define graders-mapping-file "grader-mapping.rktd")
(define grader-config-file (build-path grader-server-dir "config.rktd"))
(define data-dir (build-path base-dir "data"))
(define checker-file "checker.rkt")
(define server-ignore-file-list
  (map build-path
       (list* checker-file
              ".DS_Store"
              (append
               (glob "*~")
               (glob "*.bak")))))
(define handin-file "handin.rkt")

;; These are absolute paths to programs that don't seem to have Racket equivalents.
(define-runtime-path chown (find-executable-path "chown"))

(define graded-part-string "part~a.rkt")

(define split-student-regexp #rx"([^+]*)(\\+([^+]*))?")

;; String -> (Values String (U String #f))
(define (split-students students-string)
  (define s (regexp-match split-student-regexp students-string))
  (values (second s) (fourth s)))
(module+ test
  (check-equal? (let-values ([(a b) (split-students "a+b")])
                  (list a b))
                (list "a" "b"))
  (check-equal? (let-values ([(a b) (split-students "a")])
                  (list a b))
                (list "a" #f)))
