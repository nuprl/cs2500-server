#lang handin-server/checker

;; PLACE THIS FILE IN ASSIGNMENT DIRECTORIES!!!

(require "../checker-lib.rkt")

(check: :eval? #f
        ;; Users is one of:
        ;; * a list of lists of strings, where each inner list represents a valid user pair
        ;; * a function that accepts a list of usernames and errors if the list is not a valid pair
        ;; * #f for assignments that accept only individual submissions
        :users valid-pairing
        :create-text? #t
        :textualize? #t
        :maxwidth 102
        :student-line "Student: {username} Lab: {Lab} Grader: {Grader}"
        :extra-lines '("Maximum points for this assignment: <+CHANGEME>")
        :output "handin.rkt")
