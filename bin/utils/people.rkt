#lang typed/racket

;; A bunch of helper functions for use by scripts in the bin/ directory

(provide (all-defined-out))

(require racket/runtime-path)

(define-runtime-path base-dir "../..")
(define students-file (build-path base-dir "students.rktd"))
(define graders-file (build-path base-dir "graders.rktd"))

(struct person ([id : String]
                [name : String]
                [email : String])
  #:transparent)

;; Add a new person to a list of people, adding them to the list
;;   To be used to add new students or graders
(: add-person (Path person -> Void))
(define (add-person file person)
  (define people
    (with-input-from-file file
      (λ ()
        (read))))
  (define new-people (cons (martial-person person) people))
  (with-output-to-file file
    (λ ()
      (write people))))

;; Martial person into a list that can be writed
(: martial-person (person -> (List String String String)))
(define (martial-person person)
  (list (person-id person) (person-name person) (person-email person)))

;; Converts a list into a valid person
(: unmartial-person ((List String String String) -> person))
(define (unmartial-person p)
  (person (first p)
          (second p)
          (third p)))

;; Get all of the people from a file
(: get-people (Path -> (Listof person)))
(define (get-people file)
  (map unmartial-person
       (with-input-from-file file
         (λ ()
           (cast (read) (Listof (List String String String)))))))
