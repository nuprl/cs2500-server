(module checker handin-server/checker
  (check: :eval? #f
          :users pairs-or-singles-with-warning
          :create-text? #t
          :textualize? #t
          :maxwidth 102
          :student-line "Student: {username} Lab: {Lab} Grader: {Grader}"
          :extra-lines '("Maximum points for this assignment: <+CHANGEME>")
          :output "handin.rkt"))
