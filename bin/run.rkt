#!/usr/bin/env racket
#lang at-exp racket

(require compiler/find-exe
         syntax/location
         ;readline/readline
         "utils/constants.rkt")

(struct server (path
                [thread #:mutable]
                [output-file #:mutable]
                [output-file-path #:mutable]
                [proc-callback #:mutable]))
(define (make-server path)
  (server path #f #f #f #f))

(define (run-server! server)
  (define s-p (server-path server))
  (define output-file-path (make-temporary-file))
  (set-server-output-file-path! server output-file-path)
  (define ret
    (thread (λ ()
              (with-output-to-file output-file-path #:exists 'append
                (λ ()
                  (set-server-output-file! server (current-output-port))
                  (parameterize ([current-directory s-p]
                                 [current-subprocess-custodian-mode 'kill])
                    (let loop ()
                      (match-define (list _ _ pid _ callback)
                        (process*/ports (current-output-port)
                                        (current-input-port)
                                        'stdout
                                        (find-exe) "-I" "handin-server"))
                      (set-server-proc-callback! server callback)
                      (sleep (* 60 10)) ; 10 minutes
                      (set-server-proc-callback! server #f)
                      (callback 'kill)
                      (loop))))))))
  (set-server-thread! server ret)
  server)

(define servers (make-hash))
(dict-set! servers 'student (run-server! (make-server student-server-dir)))
(dict-set! servers 'grader  (run-server! (make-server grader-server-dir)))

(displayln "Welcome to the CS2500 Server Interface")
(displayln "Type (help) for help")

(define prompt "> ")

(let loop ()
  (display prompt)
  (define command (read))
    #|
  (define command-str (readline "> "))
  (define command
    (with-handlers ([exn? (λ (e) (printf "Error: ~a" e))])
      (add-history command-str)
      (read (open-input-string command-str))))
|#
  (match command
    [`(help)
     (displayln
      @~a{Currently implemented commands:
        - (eval <code>)
        - (output <server>)
        - (restart <server>)
        - (list-servers)
        - (exit)})]
    [`(exit) (exit)]
    [`(eval ,expr)
     (with-handlers ([exn? (λ (e) (printf "Error: ~a" e))])
       (eval expr (module->namespace (quote-module-path))))]
    [`(output ,server-symb)
     (cond
       [(dict-has-key? servers server-symb)
        (define server (dict-ref servers server-symb))
        (define out-file (server-output-file server))
        (define out-file-path (server-output-file-path server))
        (unless (and out-file
                     (flush-output out-file)
                     out-file-path
                     (file-exists? out-file-path)
                     (displayln (file->string out-file-path)))
          (displayln "WARNING: No Output file found"))]
       [else (printf "Server ~a does not exist.~n" server)])]
    [`(restart ,server-symb)
     (cond
       [(dict-has-key? servers server-symb)
        (define server (dict-ref servers server-symb))
        (define callback (server-proc-callback server))
        (and callback (callback 'kill))
        (kill-thread (server-thread server))
        (run-server! server)]
       [else (printf "Server ~a does not exist.~n" server)])]
    [`(list-servers) (displayln servers)]
    [_ (printf "Command not found: ~a~n" command)])
  (loop))
