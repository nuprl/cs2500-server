#!/usr/bin/env racket
#lang racket

(require racket/cmdline
         file/unzip
         "utils/constants.rkt"
         "utils/data-parse.rkt")

(define-values (assignment part-count)
  (command-line
   #:args (assignment parts)
   (values assignment (string->number parts))))

(unless (and (integer? part-count) (> part-count 0))
  (error 'part-count "Part count not positive integer: ~a" part-count))

(define grader-assignment-dir (build-path grader-server-dir assignment))
(define grader-assignment-backup-dir
  (build-path grader-server-dir(format "~a-backup" assignment)))
(define student-return-dir
  (build-path student-server-dir (format "~a-grades" assignment)))

(unless (directory-exists? grader-assignment-dir)
  (error 'submission->grader
         "assignment ~a does not exist in grades-server"
         assignment))

(unless (directory-exists? student-return-dir)
  (error 'submission->grader
         "assignment ~a does not exist in student-server"
         assignment))

;; Make a backup of directory before we begin
(delete-directory/files grader-assignment-backup-dir #:must-exist? #f)
(copy-directory/files grader-assignment-dir grader-assignment-backup-dir)

; Do the copying for every grader
(with-handlers ([exn:fail?
                 (λ (e)
                   ; Replace folder with backup
                   (displayln (~a "Error occurred, reverting lab folder to its"
                                  "original state"))
                   (delete-directory/files grader-assignment-dir #:must-exist? #f)
                   (copy-directory/files grader-assignment-backup-dir grader-assignment-dir)
                   
                   ; Re-raise exception
                   (raise e))])
  (parameterize ([current-directory grader-assignment-dir])
    (for ([grader (directory-list grader-assignment-dir)])
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
        (rename-file-or-directory "grades.zip" (build-path grader "grades.zip"))
        
        ;; Return to students
        (for ([student-path (directory-list grader
                                            #:build? #t)])
          (define student (last (explode-path student-path)))
          (define return-path (build-path student-return-dir student))
          (copy-directory/files student-path return-path))))))
