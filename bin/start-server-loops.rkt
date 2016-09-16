#!/usr/bin/env racket
#lang racket

(require
 compiler/find-exe
 "utils/constants.rkt")

(define (run-server-loop server-dir)
  (thread
   (lambda ()
     (parameterize ([current-directory server-dir]
                    [current-subprocess-custodian-mode 'kill])
       (let loop ()
         (match-define (list _ _ pid _ callback)
           (process*/ports (current-output-port)
                           (current-input-port)
                           'stdout
                           (find-exe) "-I" "handin-server"))
         (sleep (* 60 10)) ; 10 minutes
         (callback 'kill)
         (loop))))))

(run-server-loop student-server-dir)
(run-server-loop grader-server-dir)
(sync never-evt)
