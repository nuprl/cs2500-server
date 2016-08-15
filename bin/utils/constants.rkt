#lang racket

(provide (all-defined-out))

(require racket/runtime-path)

;; These constants assume the github structure has not changed
;;   adjust them if needed
(define-runtime-path utils-dir ".")
(define base-dir (build-path utils-dir ".." ".."))
(define student-server-dir (build-path base-dir "student-server"))
(define grader-server-dir (build-path base-dir "grades-server"))
(define pairing-file "pairings.rktd")
(define checker-file "checker.rkt")
(define graders-mapping-file "graders.rktd")
(define server-ignore-file-list/strings
  (list checker-file
        pairing-file
        graders-mapping-file))
(define server-ignore-file-list
  (append* (for/list ([i server-ignore-file-list/strings])
             (list (build-path i) (build-path (format "~a~~" i))))))
(define handin-file "handin.rkt")
