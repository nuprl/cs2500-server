#!/usr/bin/env racket

#lang racket

;;; Constructs an assignment for the users students.

;;; IMPORTANT!!!
;;; This does NOT add the assignment to the config files for the graders
;;; or students servers. You must do that yourself.

(require racket/cmdline
         "utils/constants.rkt"
         "utils/data-parse.rkt")

(define-values (assignment part-count)
  (command-line
   #:args (assignment parts)
   (values assignment (string->number parts))))

(unless (and (integer? part-count) (> part-count 0))
  (error 'part-count "Part count not positive integer: ~a" part-count))

(for ([i (in-range 1 (add1 part-count))])
  (define assignment-path 
   (build-path student-server-dir (format "~a-part~a" assignment i)))
  (make-directory* assignment-path)
  (copy-file (build-path data-dir checker-file)
             (build-path assignment-path checker-file))
  (copy-file (build-path data-dir graders-mapping-file)
             (build-path assignment-path graders-mapping-file))
  (copy-file (build-path data-dir pairing-file)
             (build-path assignment-path pairing-file)))

(displayln
 "Remember to update the config files for the student and grader servers manually!!!")
