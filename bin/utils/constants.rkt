#lang racket

(provide (all-defined-out))

(require racket/runtime-path)

;; These constants assume the github structure has not changed
;;   adjust them if needed
(define-runtime-path utils-dir ".")
(define base-dir (build-path utils-dir ".." ".."))
(define student-server-dir (build-path base-dir "student-server"))
(define student-config-file (build-path student-server-dir "config.rktd"))
(define grader-server-dir (build-path base-dir "grades-server"))
(define grader-config-file (build-path grader-server-dir "config.rktd"))
(define data-dir (build-path base-dir "data"))
(define checker-file "checker.rkt")
(define server-ignore-file-list/strings
  (list checker-file))
(define server-ignore-file-list
  (append* (for/list ([i server-ignore-file-list/strings])
             (list (build-path i) (build-path (format "~a~~" i))))))
(define handin-file "handin.rkt")

;; These are absolute paths to programs that don't seem to have Racket equivalents.
(define-runtime-path chown (find-executable-path "chown"))
