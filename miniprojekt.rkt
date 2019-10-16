#lang racket

(define students (car (file->list "students.txt")))


#| HELPER FUNCTIONS |#

;alias for sort function
(define (sort-students key-function lst)
  (sort lst #:key key-function string<?))

;returns a random element from a list
(define (get-random-element lst)
  (list-ref lst (random (length lst))))

;removes all members in a group from a list of students
(define (remove-group-from-list gl sl)
  (if (null? gl)
      sl
      (remove-group-from-list (cdr gl) (remove (car gl) sl))))

;checks if input is a student and removes group id from student
(define (check-if-student-and-remove-group st)
  (cond [(not (student? st)) (invalid-student-error)]
        [(is-in-group? st) (cdr st)]
        [else st]))

;adds all elements in a list together and returns sum
(define (add-elements-together gsl)
  (cond [(null? gsl) 0]
        [(not (exact-nonnegative-integer? (car gsl))) (error "Contained non-positive numbers")]
        [else (add-elements-together-helper (cdr gsl) (car gsl))]))

(define (add-elements-together-helper gsl x)
  (cond [(null? gsl) x]
        [(not (exact-nonnegative-integer? (car gsl))) (error "Contained non-positive numbers")]
        [else (add-elements-together-helper (cdr gsl) (+ x (car gsl)))]))


#| ERRORS |#

(define (invalid-student-error) (error "Invalid student"))


#| CONSTRUCTOR FUNCTIONS |#

#| grouping |#

;constructs a group by counting from 1 to k until it has grouped all students
(define (group-by-counting sl k)
  (if (null? sl)
      sl
      (let ([grouping (list (cons 1 (car sl)))])
        (group-by-counting-helper grouping (cdr sl) 1 k))))

(define (group-by-counting-helper gsl sl i k)
  (if (null? sl)
    gsl
    (let* ([id (+ (remainder i k) 1)]
          [grouping (append gsl (list (cons id (car sl))))])
           (group-by-counting-helper grouping (cdr sl) id k))))

;constructs a balanced grouping by counting      sl = student list   k = group size
(define (balanced-grouping-by-counting sl k)
  (let* ([sorted-by-etni (sort-students nationality-of-student students)]
         [sorted (sort-students gender-of-student sorted-by-etni)])
    (group-by-counting sorted k)))

;constructs a random grouping from a list of students and a list of group sizes
(define (random-grouping sl gsl)
  (if (= (add-elements-together gsl) (length sl))
      (random-grouping-helper '() sl gsl 1)
      (error "Combined amount of group members is not equal to number of students")))
      
(define (random-grouping-helper groups sl gsl id)
  (cond [(null? gsl) groups]
        [else (let* ([group (random-group sl (car gsl))]
                     [new-sl (remove-group-from-list group sl)]
                     [new-groups (append groups (construct-group id group))])
                (random-grouping-helper new-groups new-sl (cdr gsl) (+ id 1)))]))


;constructs a random grouping that fulfills a predicate  sl = student list   gsl = list of group sizes
;if the function is unsuccessfull in creating the specified amount of groups it just returns the groups that it has
;formed so far that fulfill the predicate
(define (random-grouping-with-predicate predicate sl gsl)
  (if (= (add-elements-together gsl) (length sl))
      (random-grouping-with-predicate-helper predicate '() sl gsl 1)
      (error "Combined amount of group members is not equal to number of students")))

(define (random-grouping-with-predicate-helper predicate groups sl gsl id)
  (cond [(null? gsl) groups]
        [else (let ([group (form-random-fulfilling-group predicate sl (car gsl) 1000)])
                (if (null? group)
                    groups
                    (let ([new-sl (remove-group-from-list group sl)]
                          [new-groups (append groups (construct-group id group))])
                      (random-grouping-with-predicate-helper predicate new-groups new-sl (cdr gsl) (+ id 1)))))]))
#| group |#

;constructs a group
(define (construct-group id sl)
  (map (lambda (member)
         (cons id member))
       sl))


;forms a random group that fulfills a predicate
(define (form-random-fulfilling-group predicate sl size tries)
  (let ([group (random-group sl size)])
    (cond [(predicate group) group]
          [(= tries 0) '()]
          [else (form-random-fulfilling-group predicate sl size (- tries 1))])))

; returns a random group from the list of students by using the helper function
(define (random-group sl size)
  (random-group-helper '() sl size))

;returns a random group from the list of students give    group= group to return  sl = list of students  mm = missing number of students in group
(define (random-group-helper group sl mm)
  (if (= mm 0)
      group
      (let* ([member (get-random-element sl)]
            [new-sl (remove member sl)])
        (random-group-helper (append group (list member)) new-sl (- mm 1)))))

#| students |#

(define (construct-student id name gender etni age)
  (let ([st (cons id (cons name (cons gender (cons etni (list age)))))])
    (if (student? st)
        st
        (error "Not a student!"))))


#| SELECTOR FUNCTIONS |#

#| groupings |#

(define (group-from-grouping id grouping [group '()])
  (cond [(null? grouping) group]
        [(= id (groupid-of-student (car grouping))) (group-from-grouping id (cdr grouping) (append group (list (car grouping))))]
        [else (group-from-grouping id (cdr grouping) group)]))

(define (groups-in-grouping grouping [group-ids '()])
  (cond [(null? grouping) group-ids]
        [(member (groupid-of-student (car grouping)) group-ids) (groups-in-grouping (cdr grouping) group-ids)]
        [else (groups-in-grouping (cdr grouping) (append group-ids (list (groupid-of-student (car grouping)))))]))

(define (number-of-groups-in-grouping grouping)
  (length (groups-in-grouping grouping)))

(define (max-group-size-in-grouping grouping)
  (let ([group-ids (groups-in-grouping grouping)])
    (max-group-size-in-grouping-helper grouping (cdr group-ids) (length (group-from-grouping (car group-ids) grouping)))))

(define (max-group-size-in-grouping-helper grouping group-ids max-size)
  (if (null? group-ids)
      max-size
      (let ([size (length (group-from-grouping (car group-ids) grouping))])
        (if (> size max-size)
        (max-group-size-in-grouping-helper grouping (cdr group-ids) size)
        (max-group-size-in-grouping-helper grouping (cdr group-ids) max-size)))))

(define (min-group-size-in-grouping grouping)
  (let ([group-ids (groups-in-grouping grouping)])
    (min-group-size-in-grouping-helper grouping (cdr group-ids) (length (group-from-grouping (car group-ids) grouping)))))

(define (min-group-size-in-grouping-helper grouping group-ids min-size)
  (if (null? group-ids)
      min-size
      (let ([size (length (group-from-grouping (car group-ids) grouping))])
        (if (< size min-size)
        (min-group-size-in-grouping-helper grouping (cdr group-ids) size)
        (min-group-size-in-grouping-helper grouping (cdr group-ids) min-size)))))
#| groups |#

;gets the ids of all group members
(define (ids-of-group-members group [ids '()])
  (if (null? group)
      ids
      (ids-of-group-members (cdr group)(append ids (list (id-of-student (car group)))))))

;gets the name of all group members
(define (name-of-group-members group [names '()])
  (if (null? group)
      names
      (name-of-group-members (cdr group)(append names (list (name-of-student (car group)))))))

;gets the gender of all group members
(define (gender-of-group-members group [genders '()])
  (if (null? group)
      genders
      (gender-of-group-members (cdr group)(append genders (list (gender-of-student (car group)))))))

;gets the gender of all group members
(define (nationality-of-group-members group [nationalities '()])
  (if (null? group)
      nationalities
      (nationality-of-group-members (cdr group)(append nationalities (list (nationality-of-student (car group)))))))

;gets the ages of all group members
(define (ages-of-group-members group [ages '()])
  (if (null? group)
      ages
      (ages-of-group-members (cdr group)(append ages (list (age-of-student (car group)))))))

#| students |#

;returns group id of student
(define (groupid-of-student st)
  (cond [(null? st) invalid-student-error]
        [(not (is-in-group? st)) (error "Student not in a group")]
        [else (car st)]))

;returns id of student
(define (id-of-student st)
  (let ([student (check-if-student-and-remove-group st)])
    (if(null? student)
       invalid-student-error
       (car student))))

;returns name of student
(define (name-of-student st)
   (let ([student (check-if-student-and-remove-group st)])
    (if(null? student)
       invalid-student-error
       (cadr student))))

;returns gender of student
(define (gender-of-student st)
   (let ([student (check-if-student-and-remove-group st)])
    (if(null? student)
       invalid-student-error
       (caddr student))))

;returns nationality of student
(define (nationality-of-student st)
  (let ([student (check-if-student-and-remove-group st)])
    (if(null? student)
       invalid-student-error
       (cadddr student))))

;returns age of student
(define (age-of-student st)
   (let ([student (check-if-student-and-remove-group st)])
    (if(null? student)
       invalid-student-error
       (cadr (cdddr student)))))


#| PREDICATE FUNCTIONS |#

(define (atleast-3-students-22-years-older group)
  (atleast-n-students-a-years-older 3 22 group))

(define (atleast-n-students-a-years-older n a group [ns 0])
  (cond [(null? group) (>= ns n)]
        [(>= (age-of-student (car group)) a) (atleast-n-students-a-years-older n a (cdr group) (+ ns 1))]
        [else (atleast-n-students-a-years-older n a (cdr group) ns)]))

(define (no-group-members-are-of-same-age group)
  (not (check-duplicates (ages-of-group-members group '()))))


;(define (group? group)
 ; (
      
;(define (group?-helper group id)
  ;(if (and (student? (car group) ()))

;returns true if all group members are female
(define (all-group-members-are-female group)
  (andmap female? group))

;returns true if one group member is female
(define (has-one-female group)
  (ormap female? group))


;returns true if the student is a female
(define (female? st)
  (string=? (gender-of-student st) "female"))

;returns true if the student is in a group
(define (is-in-group? student)
  (number? (car student)))

;returns true if input is a student
(define (student? st)
  (cond [(number? (car st)) (student? (cdr st))]
        [(not (= (length st) 5)) #f]
        [else (student? (cdr st))
              (let ([id (car st)]
                    [name (cadr st)]
                    [gender (caddr st)]
                    [etni (cadddr st)]
                    [age (cadr (cdddr st))])
                (and (string? id)
                     (string? name)
                     (string? gender)
                     (or (string=? gender "male") (string=? gender "female"))
                     (string? etni)
                     (and (number? age) (positive? age))))]))