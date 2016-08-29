#lang racket

(provide (all-defined-out))

(require racket/runtime-path)

;; These constants assume the github structure has not changed
;;   adjust them if needed
(define-runtime-path utils-dir ".")
(define base-dir (build-path utils-dir ".." ".."))
(define student-server-dir (build-path base-dir "student-server"))
(define grader-server-dir (build-path base-dir "grades-server"))
(define data-dir (build-path base-dir "data"))
(define pairing-file "pairings.rktd")
(define checker-file "checker.rkt")
(define graders-mapping-file "grader-mapping.rktd")
(define server-ignore-file-list/strings
  (list checker-file
        pairing-file
        graders-mapping-file))
(define server-ignore-file-list
  (append* (for/list ([i server-ignore-file-list/strings])
             (list (build-path i) (build-path (format "~a~~" i))))))
(define handin-file "handin.rkt")

;; These are absolute paths to programs that don't seem to have Racket equivalents.
(define-runtime-path chown (find-executable-path "chown"))
