;; This file is used to validate quiz and exam grade submissions.
(module checker handin-server/checker
  (require wxme file/unzip
           handin-server/utils
           racket)
  (check:
    :eval? #f
    :multi-file ".stuff"
    :textualize? #f
    :create-text? #f
    :untabify? #f
    :maxwidth #f
    :names-checker '("grades.rkt")
    :output "raw"
    (let ([ip (open-input-bytes submission)])
      (read-line ip)
      (let* ([ls (read ip)]
             [name (first ls)]
             [submission (second ls)]
             [ip (open-input-bytes submission)])
      (unless (equal? name "grades.rkt")
        (error "Bad file name"))
      (if (is-wxme-stream? ip)
          (error "Please submit in plain text and don't use the DrRacket plugin to submit. See the ta-tutor-info page")
          (let ([ls (read ip)])
            (if (and (list? ls)
                (begin
                  (unless (equal? ls (remove-duplicates ls #:key first))
                    (error "Grades list contains multiple entries for some student"))
                  (andmap (lambda (x)
                            (and (symbol? (first x)) (exact? (second x))))
                  ls)))
              (with-output-to-file name (thunk (display submission)))
              (begin
                (error "Please submit in a `read'able list of (cons Symbol?  Exact-Number?)")
                #f))))))))
