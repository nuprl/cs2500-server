#lang handin-server/checker

(require "../checker-lib.rkt")

(check: :eval? #t
        :users valid-pairing
        :create-text? #t
        :textualize? #t
        :maxwidth 80
        :student-line "Student: {username} Lab: {Lab} Grader: {Grader}"
        :extra-lines '("Maximum points for this assignment: <+CHANGEME>")
        :output "handin.rkt")
