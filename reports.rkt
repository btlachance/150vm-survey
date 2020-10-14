#lang at-exp racket
(require csv-reading)

(struct report
  (name
   finished-survey?
   skills
   component
   interesting
   why-interesting
   uninteresting
   ideal-lang
   comp40
   comp105
   with-whom
   timezone
   async-options
   async-avoid
   conditions
   leadup-personal
   leadup-class
   aftermath-personal
   aftermath-class
   why-taking
   most-appealing
   worries-anxieties
   anything-else))

;; survey-skills-table is a (list (cons symbol string)) of the skills
;; students were asked to report their confidence in. The symbol
;; uniquely identifies a skill, and the string is the description
;; students were given.
(define survey-skills-table
  '((inv . "Understanding data-structure invariants")
    (fun . "Using `curry` and function composition (`o`) with functions")
    (fold . "Programming with folds")
    (mon . "Programming with monads (which you may not have heard of)")
    (pat . "Pattern matching with algebraic data types")
    (mlty . "Reading and understanding ML types and signatures")
    (otherml . "Reading other people's ML code")
    (instml . "Installing Moscow ML and possibly MLton on your own personal computer")
    (xlateopsem . "Translating operational semantics into code")
    (enum . "Programming with C/C++ enums and `switch` statements")
    (cpre . "Writing C preprocessor macros")
    (valgrind . "Using `valgrind` to validate good memory usage")
    (otherc . "Reading other people's C code")
    (instc . "Installing a C compiler (like `clang` or `gcc`), `valgrind`, and `gdb` on your own personal computer")
    (txtfile . "Getting data out of text-based file formats")
    (changerep . "Other ways of changing the representation of data without changing the information content (e.g., changing the representation of an image, or of a computer program)")
    (compile . "Using compile scripts")
    (make . "Using Makefiles")))

;; survey-skills is a (list symbol) representing the names of
;; the skills, in the order they were listed in the survey
(define survey-skills (map car survey-skills-table))

;; (describe-skill symbol) returns the survey description of the
;; skill named by the symbol
(define describe-skill
  (let ([s* (make-immutable-hash survey-skills-table)])
    (lambda (n) (hash-ref s* n))))
  

(define (row->report row)
  (define items (list->vector row))

  ;; (i n) returns the nth item in the row
  (define (i n) (vector-ref items n))

  ;; (?i n) returns either the nth item in the row or 'N/A if that
  ;; item is "" (? to suggest optional, not Boolean)
  (define (?i n)
    (match (i n)
      ["" 'N/A]
      [v v]))

  ;; (s n) returns the nth skill where n is a natural number or 'N/A
  (define (s n)
    (define first-skill-idx 18)
    (match (i (+ first-skill-idx n))
      ["" 'N/A]
      ["Not at all confident" '|Not at all|]
      ["Slightly confident" 'Slightly]
      ["Somewhat confident" 'Somewhat]
      ["Fairly confident" 'Fairly]
      ["Confident" 'Confident]))

  (define name (?i 17))

  (define skills
    (for/hash ([name (in-list survey-skills)]
               [n (in-naturals)])
      (values name (s n))))

  (define component
    (match (i 36)
      ["" 'N/A]
      ["A little more excited about the SVM" 'SVM+]
      ["Much more excited about the SVM" 'SVM++]
      ["A little more excited about the UFT" 'UFT+]
      ["Much more excited about the UFT" 'UFT++]
      ["Equally excited about both" '|Both equal|]))

  (define (three-answers as)
    (match as
      ['() (list 'N/A 'N/A 'N/A)]
      [`((,item0)) (list item0 'N/A 'N/A)]
      [`((,item0 ,item1)) (list item0 item1 'N/A)]
      [`((,item0 ,item1 ,item2 ,_ ...)) (list item0 item1 item2)]))
  
  (define interesting (three-answers (csv->list (i 37))))
  (define uninteresting (three-answers (csv->list (i 38))))

  (define comp40
    (match* ((i 78) (i 79))
      [("Yes" "") '|At Tufts|]
      [("No" (and (not "") other)) other]
      [("" "") 'N/A]))

  (define comp105
    (match* ((i 80) (i 81))
      [("Yes" (or "" "N/A")) '|At Tufts|]
      [("No" (and (not "") other)) other]
      [("" "") 'N/A]))

  (define with-whom
    (match (i 85)
      ["" 'N/A]
      ["Almost all work solo" '|Almost all solo|]
      ["Most work solo" '|Most solo|]
      ["A healthy mix of solo and group work" '|Healthy mix of both|]
      ["Most work with a group or partner" '|Most w/group or partner|]
      ["Almost all work with a group or partner" '|Almost all w/group or partner|]))

  (define (format-other o) (format "Other: ~a" o))
  (define conditions
    (match* ((i 83) (i 84))
      [("" "") 'N/A]
      [("" (and (not "") o)) (list (format-other o))]
      [(selected o)
       ;; qualtrics put an unescaped string w/a comma in a field of
       ;; comma-separated values
       (define unescaped-problem-pat "I will have the flexibility to work at about any time I choose, as long as it's appropriate my time zone\\.")
       (define replacement "I will have the flexibility to work at about any time I choose as long as it's appropriate my time zone.")
       (define knowns
         (first (csv->list (regexp-replace unescaped-problem-pat selected replacement))))

       (append 
        (sort knowns string<?)
        (if (equal? "" o) '() (list (format-other o))))]))

  (define async-options
    (if (eq? (i 86) "")
        #f
        (let* ([ranks+responses
                (list (list (i 86) "Canvas discussion boards")
                      (list (i 87) "Discord (from discord.com)")
                      (list (i 88) "Discourse (from discourse.org)")
                      (list (i 89) "Google Jamboards")
                      (list (i 90) "Piazza")
                      (list (i 91) "Slack")
                      (list (i 92) "Stack overflow teams")
                      (list (i 93) "Another alternative that you name below"))]
               [sorted (map second (sort ranks+responses < #:key (compose string->number first)))])

          (match* ((first sorted) (second sorted) (i 94))
            [("Another alternative that you name below" second alt)
             (list (format "~a (named)" alt) second)]
            [(first "Another alternative that you name below" alt)
             (list first (format "~a (named)" alt))]
            [("" "" _)
             (list 'N/A 'N/A)]
            [(first "" _)
             (list first 'N/A)]
            [(first second _)
             (list first second)]))))

  (report name
          (match (i 6)
            ["True" #t]
            ["False" #f])
          skills
          component
          interesting
          (?i 76)            ;why-interesting
          uninteresting
          (?i 77)            ;ideal-lang
          comp40
          comp105
          with-whom
          (?i 82)            ;timezone
          async-options
          (?i 95)            ;async-avoid
          conditions
          (?i 96)            ;leadup-personal
          (?i 97)            ;leadup-class
          (?i 98)            ;aftermath-personal
          (?i 99)            ;aftermath-class
          (?i 100)           ;why-taking
          (?i 101)           ;most-appealing
          (?i 102)           ;worries-anxieties
          (?i 103)           ;anything-else
          ))

(define empty-row
  `(,@(build-list 6 (lambda _ 'filler))
    "True"
    ,@(build-list 10 (lambda _ 'filler))
    Name
    
    ,@(build-list 19 (lambda _ ""))
    "interest0,hello"
    "uninterest0,world"
    "everything,else,in,the"
    ,@(build-list 36 (lambda _ 'filler))
    "it sounds fun"
    "PL/I, I heard it was cool"
    "Yes"
    ""
    "No"
    "Some other COMP 105 experience"
    "Eastern time"
    "I have a reliable computer that I can use for project work."
    "Oops I didn't mean to fill this out"
    "Most work solo"
    "1"
    "2"
    ,@(build-list 6 (lambda (n) (number->string (+ n 2))))
    ""
    "I don't recommend platform XYZ, I can't use its UI"

    ""
    ""
    ""
    ""

    ""
    ""
    ""
    ""
    ))

(module+ test
  (require rackunit)

  (define r (row->report empty-row))

  (check-not-exn (lambda () (report->student-report r)))

  (check-not-exn (lambda () (report->student-report r
                                               #:skills-sort 'increasing)))

  (check-not-exn (lambda () (reports->openended-grouped (list r)))))

(define (confidence<? c1 c2)
  (define increasing (list '|Not at all| 'Slightly 'Somewhat 'Fairly 'Confident 'N/A))
  (< (index-of increasing c1) (index-of increasing c2)))

;; (skills-blurb skills order) returns a string of text for the
;; skills from some report r, where order is one of the following
;; symbols:
;; - 'survey (default)
;; - 'increasing
(define (skills-blurb skills sort-order)
  ;; (skill<? s1 s2) returns true if the reported confidence in skill
  ;; s1 is less than the reported confidence in skill s2
  (define (skill<? s1 s2) (confidence<? (hash-ref skills s1) (hash-ref skills s2)))

  (define skill-names-presentation
    (match sort-order
      ['survey survey-skills]
      ['increasing (sort survey-skills skill<?)]))

  @~a{\begin{tabular}{r p{0.80\columnwidth}}

     @(apply ~a #:separator "\n"
             (for/list ([s (in-list skill-names-presentation)])
               @~a{@hash-ref[skills s] & @describe-skill[s] \\}))

     \end{tabular}})
      
(define (report->student-report r #:skills-sort [skills-sort 'survey])
  (define (interesting i) (list-ref (report-interesting r) i))
  (define (uninteresting i) (list-ref (report-uninteresting r) i))

  @~a{
      # \huge{@report-name[r]}

      ## Skills

      @skills-blurb[(report-skills r) skills-sort]

      ## Interests

      Component: @report-component[r]

      Interesting:

      1. @interesting[0]
      2. @interesting[1]
      3. @interesting[2]

      Why: @report-why-interesting[r]

      Uninteresting:

      6. @uninteresting[0]
      7. @uninteresting[1]
      8. @uninteresting[2]

      Ideal language and why: @report-ideal-lang[r]

      ## Experience

      COMP 40: @report-comp40[r]

      COMP 105: @report-comp105[r]

      ## Working conditions

      With whom: @report-with-whom[r]

      Time zone: @report-timezone[r]

      @(if (not (report-async-options r))
           @~a{Asynchronous options: N/A}
           @~a{Asynchronous options:

               1. @(first (report-async-options r))
               2. @(second (report-async-options r))})

      Which to avoid, and why: @report-async-avoid[r]

      @(if (eq? (report-conditions r) 'N/A)
           @~a{Working conditions: N/A}
           @~a{Working conditions:

               @(apply ~a #:separator "\n" (map (lambda (c) @~a{- @c}) (report-conditions r)))})

      ## Election

      ### Leadup

      - Personal: @report-leadup-personal[r]
      - Class: @report-leadup-class[r]

      ### Aftermath

      - Personal: @report-aftermath-personal[r]
      - Class: @report-aftermath-class[r]

      ## Wrapup

      Why taking the course: @report-why-taking[r]

      Most appealing aspect: @report-most-appealing[r]

      Worries, anxieties: @report-worries-anxieties[r]

      Anything else: @report-anything-else[r]
      })

(define (reports->openended-grouped rs)
  ;; not sure this will work 100%, e.g. does the formatting break if a
  ;; v has an unindented blank line in it?
  (define (unordered-list vs)
    (apply ~a #:separator "\n\n" (map (lambda (v) @~a{- @v}) vs)))

  (define (aggregate title f #:sort [sort values])
    (define responses (sort (filter (lambda (v) (not (eq? 'N/A v))) (map f rs))))

    @~a{# @title

        @unordered-list[responses]
        })

  (define (sort-strings xs) (sort xs string<?))

  (list (aggregate "Name" report-name)
        (aggregate "Interest in features" report-why-interesting)
        (aggregate "Ideal language to implement" report-ideal-lang)
        (aggregate "Time zone" report-timezone
                   #:sort sort-strings)
        (aggregate "Asynchronous options to avoid" report-async-avoid
                   #:sort sort-strings)
        (aggregate "Personal ways to deal w/November leadup" report-leadup-personal)
        (aggregate "How classs can deal w/November leadup" report-leadup-class)
        (aggregate "Personal ways to deal w/November aftermath" report-aftermath-personal)
        (aggregate "How classs can deal w/November aftermath" report-aftermath-class)
        (aggregate "Taking the course because...?" report-why-taking)
        (aggregate "Most interesting aspect of the course" report-most-appealing)
        (aggregate "Worries? Anxieties?" report-worries-anxieties)
        (aggregate "Anything else?" report-anything-else)
        ))

(module+ main
  (define input (make-parameter #f))
  (define mode (make-parameter 'student-report)) ;; 'student-report or 'openended-grouped

  (define name "reports")
  (define (command-line-error s)
    (raise-user-error (format "~a: ~a" name s)))

  (command-line
   #:program "reports"
   #:once-any

   ["--sample" "Read data from a sample report"
               (input 'sample)]

   ["--qualtrics" f "Read Qualtrics responses from file <f>"
                  (input f)]

   #:once-any
   ["--student-report" "Render all of the response data by student (default)"
                       (mode 'student-report)]
   
   ["--openended-grouped" "Render just the open-ended responses, grouped by question"
                          (mode 'openended-grouped)]

   ["--mod1-cluster" "Render named, finished responses for module 1 clustering"
                     (mode 'mod1-cluster)])

  (unless (input)
    (command-line-error "No input specified; use one of --sample or --qualtrics <f>"))

  (define rows
    (match (input)
      ['sample (list empty-row)]
      [path
         
       (define reader (make-csv-reader (open-input-file path)))
       (match-define (list headers questions _ _ rows* ...) (csv->list reader))
       rows*]))

  (define (report<? r1 r2)
    (define (name<? s1 s2)
      (match* (s1 s2)
        [('N/A 'N/A) #f]
        [(_ 'N/A) #t]
        [('N/A _) #f]
        [(s1 s2) (string<? s1 s2)]))
    (name<? (report-name r1) (report-name r2)))
  (define reports (sort (map row->report rows) report<?))

  (define mds
    (match (mode)
      ['student-report
       (define-values (fin unfin)
         (partition report-finished-survey? reports))
       (define (student-report r) (report->student-report r #:skills-sort 'increasing))
       (map student-report (append (sort fin report<?) unfin))]

      ['openended-grouped
       (reports->openended-grouped reports)]

      ['mod1-cluster

       (define ((skill<? s) r1 r2)
         (define c1 (hash-ref (report-skills r1) s))
         (define c2 (hash-ref (report-skills r2) s))
         (confidence<? c1 c2))
       (define otherc<? (skill<? 'otherc))
       (define xlateopsem<? (skill<? 'xlateopsem))

       (define reports*
         (sort (filter (lambda (r) (not (eq? (report-name r) 'N/A)))
                       (filter report-finished-survey? reports))
               (lambda (r1 r2)
                 ;; lexicographic sort on otherc * xlateopsem
                 (or (otherc<? r1 r2)
                     (and (not (otherc<? r2 r1))
                          (not (xlateopsem<? r2 r1)))))))

       (define (flexible? r)
         (match (report-conditions r)
           ['N/A #f]
           [cs
            (findf (lambda (c) (string-prefix? c "I will have the flexibility"))
                   cs)]))
       (define-values (flex unflex) (partition flexible? reports*))

       (define (md r)
         (define (s skill) (hash-ref (report-skills r) skill))
         @~a{@report-name[r] (@report-with-whom[r])

             - Reading other people's C: @s['otherc]
             - Translating operational semantics into code:  @s['xlateopsem]


             })

       (list @~a{
                 # \Huge Flexible time

                 @(apply ~a (map md flex))

                 \newpage

                 # \Huge Not flexible time

                 @(apply ~a (map md unflex))})]))

  (display (apply ~a #:separator "\n\\clearpage\n\n" mds))
  (newline))

(module+ doc
  ;; headers
  #;((0 "StartDate")
     (1 "EndDate")
     (2 "Status")
     (3 "IPAddress")
     (4 "Progress")
     (5 "Duration (in seconds)")
     (6 "Finished")
     (7 "RecordedDate")
     (8 "ResponseId")
     (9 "RecipientLastName")
     (10 "RecipientFirstName")
     (11 "RecipientEmail")
     (12 "ExternalReference")
     (13 "LocationLatitude")
     (14 "LocationLongitude")
     (15 "DistributionChannel")
     (16 "UserLanguage")

     (17 "Name") #|first thing we asked|#

     #|confidence: Not at all confident, Slightly confident, Somewhat confident, Fairly confident, Confident|#
     #|not, slightly, somewhat, fairly, confident|#

     (18 "Skills assay_1")         #|data structure inv|#
     (19 "Skills assay_2")         #|curry and comp|#
     (20 "Skills assay_3")         #|folds|#
     (21 "Skills assay_4")         #|monads|#
     (22 "Skills assay_5")         #|pattern matching+algebraic|#
     (23 "Skills assay (row 5)_1") #|reading ml types and sigs|#
     (24 "Skills assay (row 5)_2") #|reading other people's ml code|#
     (25 "Skills assay (row 5)_3") #|installing moscowml/mlton|#
     (26 "Skills assay (row 5)_4") #|translating opsem, type rules|#
     (27 "skills assay (C-ish)_2") #|c preprocessor macros|#
     (28 "skills assay (C-ish)_2") #|valgrind to validate good memory usage|#
     (29 "skills assay (C-ish)_3") #|reading other people's c code|#
     (30 "skills assay (C-ish)_4") #|installing a c compiler|#
     (31 "skills assay (C-ish)_5") #|getting data out of text|#
     (32 "skills (last)_1")        #|other ways of changing rep|#
     (33 "skills (last)_2")        #|using compile scripts|#
     (34 "skills (last)_3")        #|using makefiles|#
     (35 "skills (last)_4")

     (36 "Preferred component_1")  #|prefs: A little more excited about the SVM, Much more excited about the UFT, Much more excited about the SVM, A little more excited about the UFT, Equally excited about both|#
     #|svm++, svm+, equal, uft+, uft++|#

     (37 "Feature interests_0_GROUP")   #|interesting, should be 3|#
     (38 "Feature interests_1_GROUP")   #|uninterstinng, should be 3|#
     (39 "Feature interests_2_GROUP")   #|other, should be 6|#


     #|can probably ignore these|#
     (40 "Feature interests_0_1_RANK")  #;exceptions
     (41 "Feature interests_0_2_RANK")  #|pattern matching|#
     (42 "Feature interests_0_3_RANK")  #;modules
     (43 "Feature interests_0_4_RANK")  #|list comprehensions|#
     (44 "Feature interests_0_5_RANK")  #|python-style generators|#
     (45 "Feature interests_0_6_RANK")  #|optional arguments/varargs|#
     (46 "Feature interests_0_7_RANK")  #|all curried|#
     (47 "Feature interests_0_8_RANK")  #|type checking|#
     (48 "Feature interests_0_9_RANK")  #|type inference|#
     (49 "Feature interests_0_10_RANK") #;objects
     (50 "Feature interests_0_11_RANK") #;classes
     (51 "Feature interests_0_12_RANK") #;concurrency
     (52 "Feature interests_1_1_RANK")  
     (53 "Feature interests_1_2_RANK")  
     (54 "Feature interests_1_3_RANK")
     (55 "Feature interests_1_4_RANK")
     (56 "Feature interests_1_5_RANK")
     (57 "Feature interests_1_6_RANK")
     (58 "Feature interests_1_7_RANK")
     (59 "Feature interests_1_8_RANK")
     (60 "Feature interests_1_9_RANK")
     (61 "Feature interests_1_10_RANK")
     (62 "Feature interests_1_11_RANK")
     (63 "Feature interests_1_12_RANK")
     (64 "Feature interests_2_1_RANK")
     (65 "Feature interests_2_2_RANK")
     (66 "Feature interests_2_3_RANK")
     (67 "Feature interests_2_4_RANK")
     (68 "Feature interests_2_5_RANK")
     (69 "Feature interests_2_6_RANK")
     (70 "Feature interests_2_7_RANK")
     (71 "Feature interests_2_8_RANK")
     (72 "Feature interests_2_9_RANK")
     (73 "Feature interests_2_10_RANK")
     (74 "Feature interests_2_11_RANK")
     (75 "Feature interests_2_12_RANK")

     (76 "Feature rationale")
     (77 "Ideal language")

     (78 "COMP 40")
     (79 "Alt 40")
     (80 "COMP 105")
     (81 "Alt 105")

     (82 "Time zone")
     (83 "Conditions minima")  #|text of checked items|#
     (84 "Conditions (other)") #|what else should i know about|#

     #|team size: Almost all work solo, Most work solo, A healthy mix of solo and group work, Most work with a group or partner, Almost all work with a group or partner|#
     (85 "Group vs Solo_1")

     #|asynchronous, top two are most important|#
     (86 "Online platforms_1")
     (87 "Online platforms_2")

     (88 "Online platforms_3")
     (89 "Online platforms_4")
     (90 "Online platforms_5")
     (91 "Online platforms_6")
     (92 "Online platforms_7")
     (93 "Online platforms_8")


     (94 "Named platforms")
     (95 "Avoided platforms")


     (96 "Leadup personal")
     (97 "Leadup (class)")
     (98 "Aftermath (personal)")
     (99 "Aftermath (class)")


     (100 "class why")
     (101 "class excitement")
     (102 "class worries")
     (103 "what else?")))
