#!/usr/bin/env racket
#lang racket/base
;; Warning: chunks of this file have been modified by William J. Bowman and Leif Andersen
;; for cs2500. These chunks may contradict the README without
;; warning.

(require "utils/constants.rkt")

;; ============================================================================
;; customizations

(define grades-file (build-path base-dir "grades"))
(define pset-dir    student-server-dir)
(define pic-dir     (build-path base-dir "content"))
(define grade-conf-file (build-path base-dir "grades.rkt"))

;; ============================================================================
;; Utilities

(require "utils/course-utils.rkt" xml
         racket/list racket/file racket/match scheme/nest)

(define students-ids (user-info 'ALL))
(define students (make-hash))
(define graded-homeworks '()) ; homeworks with some grades

(define inactive-dirs (map assignment<->dir (get-conf 'inactive-dirs)))

(define grade-conf
  (let ([t (make-hasheq)] [r `(file ,grade-conf-file)])
    (λ (sym) (hash-ref! t sym (λ () (dynamic-require r sym))))))
(define-syntax-rule (defines-from-grade-conf name ...)
  (begin (define name (grade-conf 'name)) ...))
(defines-from-grade-conf
  hw-to-grade? hw-title hw-max-score grade-description
  student-total-grade additional-filter+sort student-single-grade
  name-format smooth-range graded-file orig-file grade-file html-file)

(define (register-grade student/s hw grade)
  (unless (member hw graded-homeworks)
    (if (member hw inactive-dirs)
      (set! graded-homeworks (cons hw graded-homeworks))
      (error 'register-grade (string-append "got a grade for a homework that"
                                            " is not in inactive-dirs: \"~a\" "
                                            "(student/s: ~a, grade: ~a)")
             hw student/s grade)))
  (for ([student (in-list student/s)])
    (hash-set! students student
               (cons (cons hw grade)
                     (hash-ref students student '())))))

(define (percent-str x) (real->decimal-string (* x 100) 1))

(define (get-deltas xs)
  (let loop ([xs xs] [r '()])
    (if (null? xs)
      (reverse r)
      (loop (cdr xs)
            (cons (- (car xs) (if (null? (cdr xs)) 0 (cadr xs))) r)))))
(define (get-gaps12 xs)
  (if (or (apply <= xs) (apply >= xs))
    (let ([g1s (get-deltas xs)]) (values g1s (get-deltas g1s)))
    (let ([xs (map (λ (_) #f) xs)]) (values xs xs))))

(define (read-grade file max-score)
  (let* ([s (file->string file)]
         [s (regexp-replace #px"^\\s+" s "")]
         [s (regexp-replace #px"\\s+$" s "")])
    (match s
      [(regexp #px"^([0-9]+)\\s*/\\s*([0-9]+)$" (cons _ xs))
       (let ([r (map string->number xs)])
         (if (and max-score (equal? max-score (cadr r)))
           r
	   ;; What if I have 4 students taking a makeup exam, and the makeup
           ;; exam is out of 60 points, and the original exam is out of 55? I need to convert
           ;; their grade to the new scale. Except that doesn't always work out ...
	   #; (list (/ (* (/ (first r) (second r)) max-score) max-score) max-score) 
           (error 'read-grade
                  "bad grade in ~a -- got ~s, expecting \"something/~a\""
                  (build-path (current-directory) file)
                  s max-score)))]
      [(regexp #px"^(-?[0-9]+)\\s*$" (list _ x))
       (string->number x)]
      [(regexp #px"\\S+" _) (string->symbol s)]
      [_ (error 'read-grade "could not read grade from \"~a\""
                (build-path (current-directory) file))])))

(define (newer-file? file1 file2)
  (and (file-exists? file1) (file-exists? file2)
       (> (file-or-directory-modify-seconds file1)
          (file-or-directory-modify-seconds file2))))

(define (path-suffix path)
  (regexp-replace #rx"^.*[.]" (if (string? path) path (path->string path)) ""))

;; ============================================================================

(define html-style
  `(style ([type "text/css"])
     ,(make-comment
       (string-append
        "\n.comment { font-weight: bold; background-color: #FFC0C0; }\n"
        ".grade { font-weight: bold; background-color: #FFFF60;"
        " border: solid thin red; }\n"))))
(define (write-html student/s studentdir title grade max-score marked)
  (define (infos substs sep)
    (add-between (map (λ (s) (user-substs s substs)) student/s) sep))
  (define names (infos "{Full Name}" " & "))
  (call-with-output-file html-file #:exists 'truncate
    (λ (p)
      (write-xml/content
       (xexpr->xml
        `(html
          (head (title ,title": ",@names) ,html-style)
          (body ([bgcolor "white"])
            (h1 "Graded ",title" file for ",@names" ("(tt () ,studentdir)")")
            (span ([class "comment"])
              "Computed grade for this submission: "
              (span ([class "grade"])
                nbsp ,(format "~a/~a" grade max-score) nbsp))
            (hr)
            (pre ,@marked))))
       p))))

;; Utility for `collect-grade'
(define (make-reader f)
  (let ([p (open-input-file f)])
    (port-count-lines! p)
    (case-lambda
      [() (let ([line (read-line p 'any)])
            (if (string? line) (regexp-replace #rx" +$" line "") line))]
      [(x)
       (case x
         [(close) (close-input-port p)]
         [(get-line-num) (let-values ([(line col pos) (port-next-location p)])
                           line)])])))

;; This function computes a grade for an input file.  This is done using
;; special markup -- which is any text following a ";>" (or "//>" for java etc)
;; comment.  The markup itself can contain <+N> or <-N> to add to the grade
;; (which begins at 0), or <*N%> to factor the final grade by some percentage.
;; It returns a second value that is a list of xexprs that make the marked html
;; result.  It also creates an HTML version of the graded file.
(define (collect-grade)
  (define grade 0)
  (define factor 1)
  (define markup-re
    (let ([graded* (path-suffix graded-file)]
          [orig*   (path-suffix orig-file)])
      (when (and orig* (not (equal? orig* graded*)))
        (error 'collect-grade "got files with incompatible suffixes: ~a ~a"
               graded-file orig-file))
      (case (string->symbol (string-downcase graded*))
        [(java c cc c++) "//+>"]
        [else ";+>"])))
  (define markup-only-re (regexp (string-append "^[ \t]*" markup-re)))
  (define line-markup-re
    (regexp (string-append "^(.*?)([ \t]*)(" markup-re ".*?)?[ \t]*$")))
  (define gin (make-reader graded-file))
  (define oin (and (file-exists? orig-file) ; orig is only used if it exists
                   (make-reader orig-file)))
  (define html '())
  (define (html! x . style)
    (unless (equal? x "")
      (set! html (cons (if (null? style)
                         x `(span ([class ,(format "~a" (car style))]) ,x))
                       html))))
  (let loop ([gline (gin)] [oline (and oin (oin))])
    (cond
      [(and (eof-object? gline) (or (not oin) (eof-object? oline))) 'done]
      ;; skip empty or ;>-lines (or //>) in original file (student names etc)
      [(and (string? oline)
            (or (equal? "" oline) (regexp-match markup-only-re oline)))
       (loop gline (oin))]
      [(eof-object? gline)
       (error 'collect-grade "premature eof in ~a" graded-file)]
      [else
       (match gline
         [(regexp line-markup-re (list _ prefix space markup))
          (html! (string-append prefix space))
          (let loop ([str (or markup "")])
            (match str
              [(regexp #rx"^(.*?)(<[^@<> \tA-Z]*[0-9][^@<> \tA-Z]*>)(.*)$"
                       (list _ prefix markup rest))
               (html! prefix 'comment)
               (html! markup 'grade)
               (match markup
                 [(regexp #rx"^<([+-][0-9]+(?:[.][0-9]+)?)>$" (list _ g))
                  (set! grade (+ grade (string->number g)))]
                 [(regexp #rx"^<[*]([0-9]+)%>$" (list _ f))
                  (set! factor (* factor (/ (string->number f) 100)))]
                 [(regexp #rx"^<[*](0)>$" (list _ f))
                  (set! factor (* factor (string->number f)))]
                 ;; Ignore Sinan's bogus markups
		 [(regexp #rx"^<[0-9]+/[0-9]+>$" _) (void)]
                 [else (error 'collect-grade "bad grade markup in ~s: ~e"
                              graded-file str)])
               (loop rest)]
              [_ (html! str 'comment)]))
          (html! "\n")
          ;; (printf ">>> ~s ~s\n    ~s\n" prefix markup oline)
          (cond
            [(not oin) (loop (gin) oin)] ; no original to compare against
            ;; continue if lines are equal ignoring meta markup
            [(equal? prefix oline) (loop (gin) (oin))]
            ;; skip markup-only lines and empty lines in graded file
            [(regexp-match? #rx"^[ \t]*$" prefix) (loop (gin) oline)]
            [else (loop (gin) (oin)) #;(error 'collect-grade "content mismatch: ~a:~a, ~a:~a"
                         graded-file (sub1 (gin 'get-line-num))
                         orig-file (sub1 (oin 'get-line-num)))])]
         ;; the above should always match
         [_ (error 'collect-grade "internal error in regexps")])]))
  (gin 'close) (when oin (oin 'close))
  (values (inexact->exact (ceiling (* grade factor))) (reverse html)))

(define (find-all-grades)
  (nest ([parameterize ([current-directory pset-dir])]
         [for ([hwdir (in-list (ls directory-exists?))])]
         [when (and (hw-to-grade? hwdir) ; allowed by configuration file
                    (member hwdir inactive-dirs))]
         [let ([title (hw-title hwdir)] [max-score (hw-max-score hwdir)])]
         [begin (printf "  > ~a\n" title)]
         [parameterize ([current-directory hwdir])]
         [for ([studentdir (in-list (ls directory-exists?))])]
         [parameterize ([current-directory studentdir])]
         [let ([student/s (regexp-split #rx"[+]" studentdir)])]
         [let ([student/s (map string->symbol student/s)])]
         [let ([snum (count (λ (x) (memq x students-ids)) student/s)])]
         [let ([register-grade
                (λ ([grade (read-grade grade-file max-score)])
                  (register-grade student/s hwdir grade))])]
         [when (snum . > . 0)])
    (cond
      [(snum . < . (length student/s))
       (error (format "ambiguous directory \"~a\" -- only some known students"
                      studentdir))]
      [(newer-file? orig-file graded-file)
       (error (format "~a is newer than ~a in ~a"
                      orig-file graded-file studentdir))]
      [(and (newer-file? grade-file graded-file)
            (newer-file? html-file  graded-file))
       ;; we already have a valid grade
       (register-grade)]
      [(file-exists? graded-file)
       ;; need to compute a grade, using orig-file to compare if it's there
       (printf "    ... ~a\n" studentdir)
       (let-values ([(grade html) (collect-grade)])
         (display-to-file (format "~a/~a" grade max-score)
                          grade-file #:exists 'truncate)
         (write-html student/s studentdir title grade max-score html)
         (register-grade (list grade max-score)))]
      ;; explicitly set grade
      [(file-exists? grade-file) (register-grade)]
      ;; not graded yet
      [(file-exists? orig-file) (register-grade '??)]
      [else (printf "    skipping: ~a (no graded file)\n" studentdir)]))
  ;; sort the graded-homeworks list according to the inactive-dirs list
  (set! graded-homeworks
        (filter (λ (x) (member x graded-homeworks)) inactive-dirs)))

(define (compute-all-grades)
  (for/list ([(student grades) (in-hash students)])
    (define (info k) (user-info student k))
    (define hws+grades
      (for/list ([hwname (in-list graded-homeworks)])
        (define grade (cond [(assoc hwname grades) => cdr] [else #f]))
        (cons hwname (student-single-grade hwname grade student info))))
    (define total (student-total-grade hws+grades student info))
    (list* student total (map cdr hws+grades))))

(define (write-student-summary student+total+grades)
  (define student (car student+total+grades))
  (define total   (cadr student+total+grades))
  (define grades  (cddr student+total+grades))
  (define total%  (format "~~~a%" (percent-str total)))
  (define title
    (user-substs student
                 "Summary for {Full Name} <{Email}> [{username}]"))
  (define total-name "Estimated Grade:")
  (define table
    (for/list ([hw (in-list graded-homeworks)] [grade (in-list grades)])
      (list " "
            (format "~a:" (hw-title hw))
            (if (number? grade) (round (* 100 grade)) grade)
            (format "/~a" (hw-max-score hw))
            (cond [(grade-description grade) => (λ (d) (format "(~a)" d))]
                  [else ""]))))
  (define sep
    (let ([-- (filler "-")])
      `("" ,(make-string (- (string-length total-name) 2) #\-) ,-- ,-- ,--)))
  (define pads '(l r r l l))
  (define dir (build-path pset-dir "Summary" (format "~a" student)))
  (make-directory* dir)
  (parameterize ([current-directory dir])
    (with-output-to-file "Grades.txt" #:exists 'truncate
      (λ ()
        (printf "~a:\n\n" title)
        (print-table `(,sep ,@table ,sep) pads)
        (printf "\n~a ~a\n" total-name total%)))
    (with-output-to-file "grade" #:exists 'truncate (λ () (display total%)))))

(define (print-grades students+totals+grades*)
  (define pads `(l ,@(map (λ (_) 'r) graded-homeworks) r r r))
  (define header `("" ,@graded-homeworks "total" "gap" "gap2"))
  (define separator (let ([- (filler "-")]) (map (λ (_) -) header)))
  (define subtitle (λ (str) `(,(format ">>~a<<" str) ,@(cdr header))))
  (define percents
    `(">>percent>"
      ,@(map (λ (hw)
               (percent-str
                (student-total-grade
                 (map (λ (hw*) (cons hw* (if (equal? hw* hw) 1 0)))
                      graded-homeworks)
                 '___ (λ (k) '___))))
             graded-homeworks)
      ,(percent-str 1)
      "<<" "<<"))
  (define students+totals+grades (sort students+totals+grades* > #:key cadr))
  (define funs (map (λ (s+t+gs)
                      (λ (k)
                        (define s (car s+t+gs))
                        (match k
                          ['username      s]
                          ['total-grade   (cadr s+t+gs)]
                          ['student+total+grades s+t+gs]
                          [`(substs ,str) (user-substs s str)]
                          [_              (user-info s k)])))
                    students+totals+grades))
  (define (table-body students+totals+grades)
    (define-values [gaps1 gaps2]
      (get-gaps12 (map cadr students+totals+grades)))
    (for/list ([s+t+gs students+totals+grades] [g1 gaps1] [g2 gaps2])
      (let ([student (car s+t+gs)]
            [total   (cadr s+t+gs)]
            [grades  (cddr s+t+gs)])
        `(,(user-substs student name-format)
          ,@(map (λ (g) (if (number? g) (round (* 100 g)) g)) grades)
          ,(percent-str total)
          ,(if g1 (percent-str g1)"") ,(if g2 (percent-str g2) "")))))
  (define table
    `(,header ,percents ,separator
      ,@(table-body students+totals+grades)
      #;,@(append-map
         (λ (f+s)
           (let ([t (f+s funs)])
             `(,separator ,(subtitle (car t)) ,separator
               ,@(table-body (map (λ (f) (f 'student+total+grades))
                                  (cdr t))))))
         additional-filter+sort)
      ,separator ,percents ,header))
  (with-output-to-file grades-file #:exists 'truncate
    (λ () (print-table table pads))))

(require plot racket/math)

(define (draw-grades grades* spread title target)
  (define width 700)
  (define grades ; ignore non-numeric grades
    (sort (map (λ (g) (ceiling (* 100 g))) (filter number? grades*)) <))
  (define ming (apply min 0 grades))
  (define maxg (+ 5 (apply max 100 grades)))
  ;; translated from "grader.c" (Andrew Myers), with some hard wired options
  (define bins width)
  (define curve (make-vector bins 0.0))
  (define vec (make-vector bins 0.0))
  (define range (- maxg ming))
  (define root2pi/spread (/ 1.0 (sqrt (* 2.0 pi)) spread))
  (define 1/spread (/ 1.0 spread))
  (define (gaussian x1 x2)
    (let ([o (* (- x1 x2) 1/spread)]) (* root2pi/spread (exp (* -0.5 o o)))))
  (define (apriori x) (let ([p (/ (- x ming) range)]) (* p (- 1.0 p))))
  (for ([x (in-list grades)])
    (define weight
      (for/fold ([weight 0.0]) ([j (in-range bins)])
        (define x1 (+ ming (* (/ j (- bins 1)) range)))
        (define r (* (gaussian x x1) (apriori x1)))
        (vector-set! vec j r)
        (+ weight r)))
    ;; Weight now contains integral of function being added to curve, more
    ;; or less.  Now normalize and add to curve.
    (define iweight (/ bins range weight))
    (for ([j (in-range bins)])
      (vector-set! curve j (+ (vector-ref curve j)
                              (* iweight (vector-ref vec j))))))
  (define (f x) (vector-ref curve (round (/ (* (- bins 1) (- x ming)) range))))
  ;;
  (define len (length grades))
  (define average (ceiling (/ (apply + grades) len)))
  (define median
    (if (even? len)
      (ceiling (/ (+ (list-ref grades (sub1 (/ len 2)))
                      (list-ref grades (/ len 2)))
                   2))
      (list-ref grades (/ (sub1 len) 2))))
  (define ymax (for/fold ([m 0]) ([j (in-range bins)])
                 (max (vector-ref curve j) m)))
  (define ytop (* 1.1 ymax))
  (parameterize ([plot-x-ticks (linear-ticks #:divisors '(10) #:number 12)]
                 [plot-y-ticks no-ticks]
                 [plot-font-size 14])
    (define (choose x y) (if (< average median) (values x y) (values y x)))
    (define-values [l1  l2]  (choose "average" "median"))
    (define-values [l1x l2x] (choose average median))
    (define (decor x label anchor)
      (list (y-axis x #:ticks? #f)
            (function-label f x label #:point-size 7 #:color 3
                            #:angle (* 1/2 pi) #:anchor anchor #:alpha 0.75)))
    (plot
     #:out-file target #:width width #:height 400 #:x-label #f #:y-label #f
     (list (function-interval f (λ (x) 0) ming maxg #:y-min 0 #:y-max ytop
                              #:samples bins #:color 3)
           (for/list ([i (in-range 0 maxg 10)])
             (lines (list (vector i 0) (vector i (f i)))
                    #:color 3 #:alpha 0.5))
           (function f ming maxg #:y-min 0 #:y-max ytop
                     #:samples bins #:width 2)
           (decor l1x l1 'bottom-right) (decor l2x l2 'top-right)
           (point-label (vector (+ ming (* .01 range)) (* .99 ytop)) title
                        #:family 'modern #:size 20 #:point-sym ""
                        #:anchor 'top-left)))))

(define (create-graphs students+totals+grades)
  (define grader-exe  (find-executable-path "grader"))
  (define convert-exe (find-executable-path "convert"))
  (when (and convert-exe grader-exe)
    (for ([hw (in-list (cons 'total graded-homeworks))]
          [grades (in-list (apply map list (map cdr students+totals+grades)))])
      (define total? (eq? hw 'total))
      (define title  (if total? "Total" (hw-title hw)))
      (define target (format "~a/~a.png" pic-dir hw))
      (if (for/or ([g (in-list grades)]) (number? g))
        (begin (printf "  > ~a -> ~a...\n" title target)
               (draw-grades grades ((if total? cadr car) smooth-range)
                            title target))
        (printf "  > skipping ~a, no grades\n" title)))))

(define (run)
  (printf "*** Finding grades...\n")
  (find-all-grades)
  (printf "*** Computing grades...\n")
  (let ([students+totals+grades (compute-all-grades)])
    (printf "*** Creating summaries...\n")
    (for-each write-student-summary students+totals+grades)
    (printf "*** Creating grades table and summaries...\n")
    (print-grades students+totals+grades)
    #;(printf "*** Creating graphs...\n")
    #;(create-graphs students+totals+grades))
  (printf "*** Done.\n"))

(run)
