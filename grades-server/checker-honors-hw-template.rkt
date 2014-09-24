;; This file is used to validate honors homework submissions.
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
    :names-checker #px"honors-hw\\d+.zip"
    :output "raw"
    (let ([ip (open-input-bytes submission)])
      (read-line ip)
      (let* ([ls (read ip)]
             [name (first ls)]
             [submission (second ls)]
             [ip (open-input-bytes submission)])
      (unless (regexp-match #px"^honors-hw\\d+.zip" name)
          (error "Bad file name"))
      (unless (equal? (subbytes submission 0 2) #"PK")
          (error "Invalid zip file"))
      (unzip ip
        (lambda (entry-name dir? ip)
          (unless dir?
            (unless (regexp-match #px#"^[\\w\\.\\+-]+/graded\\.rkt" entry-name)
              (error "Invalid zip file: not all files are named graded.rkt"))
            (when (is-wxme-stream? ip) (error (format "~s is not plain text" entry-name))))))
      (with-output-to-file name (thunk (display submission)))))))
