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

(for ([i part-count])
  (define assignment-path
   (build-path student-server-dir
               (format "~a~a" assignment (integer->char (+ (char->integer #\a) i)))))
  (make-directory* assignment-path)
  (copy-file (build-path data-dir checker-file)
             (build-path assignment-path checker-file)))

(displayln
 "Remember to update the config files for the student and grader servers manually!!!")
