#!/usr/bin/env racket
#lang racket

;; Usage: ./return_grades_to_students.rkt assignment-name number-of-parts

(require racket/cmdline
         file/unzip
         "utils/constants.rkt"
         "utils/data-parse.rkt"
         "utils/parse-comments.rkt")

(define-values (assignment part-count part-to-grade)
  (command-line
   #:args (assignment parts part-to-grade)
   (values assignment (string->number parts) (string->number part-to-grade))))

(unless (and (integer? part-count) (> part-count 0))
  (error 'part-count "Part count not positive integer: ~a" part-count))

(define grader-assignment-dir (build-path grader-server-dir assignment))
(define grader-assignment-backup-dir
  (build-path grader-server-dir (format "~a-backup" assignment)))
(define student-return-dir
  (build-path student-server-dir (format "~a-grades" assignment)))

(define grader-mapping
  (read-grader-mapping-file (build-path student-server-dir graders-mapping-file)))

(unless (directory-exists? grader-assignment-dir)
  (error 'submission->grader
         "assignment ~a does not exist in grades-server"
         assignment))

(when (and (directory-exists? student-return-dir)
           (not (null? (directory-list student-return-dir))))
  (error 'submission->grader
         "assignment ~a-grades already exists in student-server"
         assignment))

(make-directory* student-return-dir)

;; Make a backup of directory before we begin
(delete-directory/files grader-assignment-backup-dir #:must-exist? #f)
(copy-directory/files grader-assignment-dir grader-assignment-backup-dir)

;; Store which graders returned what
(define return-mapping (make-hash))

; Do the copying for every grader
(parameterize ([current-directory grader-assignment-dir])
  (for ([grader (directory-list grader-assignment-dir)])
    (with-handlers ([exn:fail?
                     (λ (e)
                       ; Replace folder with backup
                       (printf (string-join (list "Error occurred, reverting graders folder to its"
                                                  "original state~n"
                                                  "Current grader: ~a~n"))
                               grader)
                       (delete-directory/files grader-assignment-dir #:must-exist? #f)
                       (copy-directory/files grader-assignment-backup-dir grader-assignment-dir)
                       (delete-directory/files student-return-dir)
                       
                       ; Re-raise exception
                       (raise e))])
      (printf "Returning grades from: ~a~n" grader)
      (unless (ormap (curry equal? grader) server-ignore-file-list)
        
        ;; Unzip the file
        (unless (file-exists? (build-path grader "grades.zip"))
          (error 'grader "Grader ~a didn't return a grades.zip file" grader))
        (delete-directory/files "grades.zip" #:must-exist? #f)
        (rename-file-or-directory (build-path grader "grades.zip") "grades.zip")
        (delete-directory/files grader #:must-exist? #f)
        (call-with-unzip
         "grades.zip"
         (λ (dir)
           (define tmp-folder (build-path dir grader))
           (if (directory-exists? tmp-folder)
               (copy-directory/files tmp-folder grader)
               (error 'grader "Grader ~a is missing their grade folder" grader))))
        
        ;; Return to students
        (for ([student (directory-list grader)])
          (when (hash-has-key? return-mapping student)
            (error 'bad-grader
                   "Both grader ~a and grader ~a submitted a solution for student ~a"
                   grader (hash-ref return-mapping student) student))
          (hash-set! return-mapping student grader)
          (copy-directory/files (build-path grader student)
                                    (build-path student-return-dir student)))

        ;; Replace grades file
        (rename-file-or-directory "grades.zip" (build-path grader "grades.zip"))))))

; Now walk through student dir to calculate their grades
(for ([student (directory-list student-return-dir)])
  (grade-file (build-path student (format "part~a.rkt" part-to-grade))
              (build-path student "grade")))
