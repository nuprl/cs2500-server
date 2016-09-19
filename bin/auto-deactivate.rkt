#!/usr/bin/env racket
#lang racket

;; Usage: ./auto-deactivate.rkt
;;
;; Moves any assignment in the student server config's active-dirs list into inactive-dirs if its
;; deadline has passed. We recommend running this script periodically from a cron job (say, every 5
;; minutes).
;;
;; Deadlines are given in student-server/deadlines.rktd (a list of pairs of strings, where the first
;; string in the pair is the name of the assignment directory, and the second is the deadline in ISO
;; 8601 extended format (see the documentation for iso8601->datetime in the Racket docs) Example:
;;
;; (("hw1a" . "2014-03-20T19:20:09.3045Z")
;;  ("hw2a" . "2014-03-27T19:30:00.0Z"))
;;
;; Active assignments without specified deadlines are left in active-dirs.

(require
 gregor
 "utils/constants.rkt")

(define (log-error exception)
  (call-with-output-file (build-path student-server-dir "auto-deactivate-errors.log")
    (lambda (file)
      (fprintf file "~a: ~v\n" (datetime->iso8601 (now)) exception))
    #:exists 'append))

(with-handlers ([(lambda (x) #t) log-error])
  (define config (call-with-input-file student-config-file read))
  (define deadlines
    (call-with-input-file (build-path student-server-dir "deadlines.rktd")
      (lambda (file)
        (for/hash ([entry (read file)])
          (values (car entry) (iso8601->datetime (cdr entry)))))))
  (define-values (new-actives new-inactives)
    (for/fold ([new-actives null]
               [new-inactives (second (assoc 'inactive-dirs config))])
              ([active-assignment (second (assoc 'active-dirs config))])
      (match (hash-ref deadlines active-assignment #f)
        [#f (values (cons active-assignment new-actives) new-inactives)]
        [deadline
         (if (datetime>? (now) deadline)
             (values new-actives (append new-inactives (list active-assignment)))
             (values (cons active-assignment new-actives) new-inactives))])))

  (define rest-of-config
    (filter (lambda (entry) (not (member (first entry) `(active-dirs inactive-dirs)))) config))

  (call-with-output-file student-config-file
    (lambda (file)
      (pretty-write `((active-dirs ,(reverse new-actives))
                      (inactive-dirs ,new-inactives)
                      ,@rest-of-config)
                    file))
    #:exists 'replace))
