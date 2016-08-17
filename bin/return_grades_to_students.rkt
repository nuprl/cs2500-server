#!/usr/bin/env racket
#lang racket

(require racket/cmdline
         "utils/constants.rkt"
         "utils/data-parse.rkt")

(define-values (assignment part-count)
  (command-line
   #:args (assignment parts)
   (values assignment (string->number parts))))

(unless (and (integer? part-count) (> part-count 0))
  (error 'part-count "Part count not positive integer: ~a" part-count))

(define grader-assignment-dir (build-path grader-server-dir assignment))
(define student-return-dir
  (build-path student-server-dir (format "~a-grades" assignment)))

(unless (directory-exists? grader-assignment-dir)
  (error 'submission->grader
         "assignment ~a does not exist in grader-server"
         assignment))

(unless (directory-exists? student-return-dir)
  (error 'submission->grader
         "assignment ~a does not exist in grader-server"
         assignment))

(for ([grader-path (directory-list grader-assignment-dir
                                   #:build? #t)])
  (define grader (last (explode-path grader-path)))
  (unless (ormap (curry equal? grader) server-ignore-file-list)
    (for ([student-path (directory-list grader-path
                                        #:build? #t)])
      (define student (last (explode-path student-path)))
      (define return-path (build-path student-return-dir student))
      (copy-directory/files student-path return-path))))
