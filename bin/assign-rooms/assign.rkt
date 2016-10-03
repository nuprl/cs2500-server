#lang at-exp racket

(require mutt
         "capacity-buckets.rkt")

(module+ test
  (require rackunit)
  (define test-set
    (student-set
     (list
      (make-student "a" "a@husky.neu.edu")
      (make-student "b" "b@husky.neu.edu")
      (make-student "c" "c@husky.neu.edu")
      (make-student "d" "d@husky.neu.edu"))))
  (define test-rooms
    (list
     (make-room 'x 1)
     (make-room 'y 2)
     (make-room 'z 1))))

(struct room (name
              capacity
              students)
  #:prefab)
(define (make-room name capacity)
  (room name capacity (student-set)))
(define (add-student-to-room! room student)
  (set-add! (room-students room) student))

(struct student (id
                 email
                 [room #:mutable])
  #:prefab)
(define (make-student id email)
  (student id email #f))

; Defining a set for students
(match-define-values
 (_ _ _ _ _ student-set _)
 (make-custom-set-types (λ (a b)
                          (equal? (student-id a)
                                  (student-id b)))
                        #:elem? student?))

;; Create students list from pairs
;; Path -> (Setof Student)
(define (pairs->students pairs-file)
  (define pairs-set (student-set))
  (with-input-from-file pairs-file
    (λ ()
      (let loop ()
        (define x (read))
        (unless (eof-object? x)
          (for ([i (in-list x)])
            (for ([j (in-list i)])
              (set-add! pairs-set (make-student j (format "~a@husky.neu.edu" j)))))
          (loop)))))
  pairs-set)

(module+ test
  (check-equal? (pairs->students "test-pairs.rktd")
                test-set))

;; Remove students who are not taking the midterm
;; (Setof Student) Path -> Void
(define (remove-students! students remove-file)
  (with-input-from-file remove-file
    (λ ()
      (let loop ()
        (define s (let ([tmp (read)])
                    (cond [(symbol? tmp) (symbol->string tmp)]
                          [else tmp])))
        (unless (eof-object? s)
          (set-remove! students (make-student s ""))
          (loop))))))

(module+ test
  (define students (student-set
                    (list (make-student "a" "")
                          (make-student "b" ""))))
  (remove-students! students "test-remove.rktd")
  (check-equal? students (student-set (list (make-student "b" "")))))

;; Assign students to their room (mutating the student objects)
;; (Listof Rooms) (Setof Student) -> Void
(define (assign! rooms students)
  (define allocations (allocate (set->list students) (map room-capacity rooms)))
  (for ([room (in-list rooms)]
        [allocation (in-list allocations)])
    (for ([student (in-list allocation)])
      (set-student-room! student room)
      (add-student-to-room! room student))))

(module+ test
  (assign! test-rooms test-set)
  (for ([i (in-set test-set)])
    (check-not-false (student-room i)))
  (for ([i (in-list test-rooms)])
    (check-not-false (room-students i))
    (check-equal? (set-count (room-students i)) (room-capacity i))))

;; Output result
;; (Listof Rooms) (Setof Students) -> Void
(define (print-results rooms students)
  (printf "There are ~a students in the class.~n" (set-count students))

  (for ([room (in-list rooms)])
    (printf "Room ~a has ~a students:~n" (room-name room) (set-count (room-students room)))
    (for ([i (in-set (room-students room))])
      (printf "  ~a~n" (student-id i)))
    (newline)))

;; Email the room assignments to the students
;; (Setof Students) -> Void
(define (email-students students)
  (for ([student (in-set students)])
    (mutt
     @~a{The room you will take your CS2500 Midterm is:

         @(student-room student)}
     #:to (student-email student)
     #:subject "CS2500 Midterm Room Assignment")))

;; Don't need to test this all the time...
#;(module+ test
  (check-equal?
   (email-students (set (student "Leif" "leif@leif.pl" "WVH 308")))
   (void)))

;; Constants
(define output-file "room-assignments.rktd")
(define pairs-file "pairs.rktd")
(define remove-students-file "removed.rktd")
(define rooms
  (list
   (make-room 'RI-200 315)
   (make-room 'SN-108 196)
   (make-room 'MU-201 187)
   (make-room 'CG-97 175)
   (make-room 'CH-103 161)
   (make-room 'SH-135 114)
   (make-room 'SH-305 114)
   (make-room 'SH-335 104)))

(module+ main
  (define students (pairs->students pairs-file))
  (remove-students! students remove-students-file)
  (assign! rooms students)
  (print-results rooms students)

  (with-output-to-file output-file
    #:exists 'replace
    (λ ()
      (pretty-write students)
      (pretty-write rooms)))
  #;(email-students students))
