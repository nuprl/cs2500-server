#!/usr/bin/env racket
#lang racket

;; Usage: ./submissions-to-graders.rkt assignment-name number-of-parts
;;
;; Copies the specified assignment to the grader server, assigning student pair submissions to graders
;; as given in the assignment's grader mapping file (student-server/grader-mapping.rktd).

(require racket/cmdline
         file/zip
         "utils/constants.rkt"
         "utils/data-parse.rkt")

(define-values (assignment part-count)
  (command-line
   #:args (assignment parts)
   (values assignment (string->number parts))))

(unless (and (integer? part-count) (> part-count 0))
  (error 'part-count "Part count not positive integer: ~a" part-count))

(define student-assignment-dirs
  (for/list ([i (in-range part-count)])
    (build-path student-server-dir
                (format "~a~a" assignment (integer->char (+ (char->integer #\a) i))))))
(define grader-assignment-dir (build-path grader-server-dir assignment))

(unless (andmap directory-exists? student-assignment-dirs)
  (error 'submission->grader
         "assignment ~a does not exist in student-server"
         assignment))

(when (and (directory-exists? grader-assignment-dir)
           (not (null? (directory-list grader-assignment-dir))))
  (error 'submission->grader
         "assignment ~a already exists in grader-server"
         assignment))

(make-directory grader-assignment-dir)

(define grader-mapping
  (read-grader-mapping-file (build-path student-server-dir graders-mapping-file)))

; Copy existing assignments to grader folder
(for ([assignment-dir (in-list student-assignment-dirs)]
      [i (in-naturals 1)])
  (for ([student-path (directory-list assignment-dir
                                      #:build? #t)])
    (define student (last (explode-path student-path)))
    (unless (ormap (curry equal? student) server-ignore-file-list)
      (define grader (find-grader grader-mapping (path->string student)))
      (define handin-path (build-path student-path handin-file))
      (define grade-dest (build-path grader-assignment-dir grader student)) 
      (make-directory* grade-dest)
      (define dest (build-path grade-dest (format "part~a.rkt" i)))
      (when (file-exists? handin-path)
        (copy-file handin-path dest))
      (system* chown "-R" ":2500admins" grade-dest))))

; zip up these new assignments for graders
(parameterize ([current-directory grader-assignment-dir])
  (for ([grader (directory-list grader-assignment-dir)])
    (apply zip "grades.zip"
           (for/list ([student (directory-list grader)]
                      #:when (directory-exists? (build-path grader student)))
             (build-path grader student)))
    (rename-file-or-directory "grades.zip" (build-path grader "grades.zip"))
    (system* chown "-R" ":2500admins" grader)))
