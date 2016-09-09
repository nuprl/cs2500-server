#!/usr/bin/env racket
#lang racket

;; Usage: ./email-passwords.rkt [OPTION] ... users-file ...
;;
;; Emails handin server users their password. Requires mutt to be installed on
;; the local machine as well as the mutt Racket package. Each users-file should
;; be in the format of the users.rktd file.
;;
;; Mutt setup differs from system to system. You may find the Racket package docs or
;; https://wiki.archlinux.org/index.php/Mutt to be helpful. It may be that you only need to set the
;; SMTP URL within your ~/.muttrc as follows, replacing $smtp_user, $smtp_pass, and $smtp_server with
;; your username, password, and domain name for the SMTP server:
;;
;; set smtp_url="smtps://$smtp_user:$smtp_pass@$smtp_server"

;; =============================================================================

(require mutt racket/file)

(define TMP "cs2500-message.txt")

(module+ main
  (require racket/cmdline)
  (define *inspiration* (make-parameter #f))
  (command-line
   #:program "email-passwords"
   #:once-each
   [("-b" "--bcc")
    b
    "Set a 'bcc' for outgoing emails."
    (*mutt-default-bcc* (in-email* b))]
   [("-i" "--inspiration")
    i
    "An inspirational quote to append to all emails"
    (*inspiration* i)]
   #:args rktd*
   (when (null? (*mutt-default-bcc*))
     (raise-user-error 'email-passwords "please specify a bcc address on the command line (./email-passwords.rkt --bcc 'you@example.com')))"))
   (for ((rktd (in-list rktd*)))
     (for/list ((row (in-list (file->value rktd))))
       (match-define `(,uname (,password-line ,fname ,email ,_ ...)) row)
       (match (list password-line email)
         [(list `(plaintext ,pword) (regexp #rx".+"))
          (define uname (car row))
          (define pword (cadr (car (cadr row))))
          (define fname (cadr (cadr row)))
          (define email (caddr (cadr row)))
          (with-output-to-file TMP #:exists 'replace
            (lambda ()
              (printf "Hello ~a,\n\n" fname)
              (printf "Your cs2500 username/password are:\n")
              (printf "    username: ~a\n" uname)
              (printf "    password: ~a\n" pword)
              (printf "\n\n")
              (when (*inspiration*)
                (displayln (*inspiration*)))
              (void)))
          (mutt TMP #:to email #:subject "[cs2500] username/password")]
         [_
          ;; We can't send non-plaintext passwords, nor can we send anything if there is no email
          ;; address
          (printf "Unable to send email for this row: ~s\n" row)])))))
