;Eric Zhou, ezzhou
;Advanced Student Custom

; Test Case Used:
; (add-course "MA" 1023 "Sturm" 399)
; (add-course "CS" 1101 "Engling" 593)
; 
; (add-student "Katie" 2949)
; (add-student "John" 6849)
; 
; (add-to-course 6849 "MA" 1023)
; 
; (largest-enrollment)


; (1.)

(define-struct course (dept course-num instructor max-enroll a-los))
; A Course is a (make-course String Number String Natural ListOfStudent) where
; dept is the department offering the course
; course-num is the course number
; insturctor is the faculty member teaching the course
; max-enroll is the maximum enrollemnt for the course
; a-los is a list of Students taking the course
; 
; A ListOfStudent is one of
; -empty
; -(cons Student ListOfStudent)


(define-struct student (name id a-loc))
; A Student is a (make-student String Number ListOfCourse) where
; name is the student's name
; id is their school ID
; a-loc is a list of Courses they are enrolled in
; 
; A ListOfCourse is one of
; -empty
; -(cons Course ListofCourse


; Test Instances for Check Expects:

(define C1 (make-course "CS" 1101 "Engling" 500 empty))
(define C2 (make-course "MA" 1024 "Sturm" 420 empty))
(define C3 (make-course "PH" 1130 "Burnham" 255 (list (make-student "Boris" 1049 empty))))
(define CL1 (list C1 C2 C3))

(define S1 (make-student "Katie" 1123 (list C1 C2)))
(define S2 (make-student "Zaq" 3957 empty))
(define S3 (make-student "Juan" 5917 (list C1 C2 C3)))
(define SL1 (list S1 S2 S3))

; (2.)

;COURSES is a ListOfCourses
(define COURSES empty)
;COURSES is a ListOfStudent
(define STUDENTS empty)

; (3.)

;Signature: String Number -> NULL
;Purpose: Adds a student to STUDENTS if their ID is valid
(define (add-student name id)
  (if (valid-id? id STUDENTS)
    (set! STUDENTS (cons (make-student name id empty) STUDENTS))
    (error "Student already exists!")))

;Signature: Number ListOfStudent -> Boolean
;Purpose: Checks if an ID matches an existing ID, returns true if ID is unique
(define (valid-id? id a-los)
  (cond
    [(empty? a-los) #true]
    [(cons? a-los) (if (= (student-id (first a-los)) id)
                   #false
                   (valid-id? id (rest a-los)))]))

(check-expect (valid-id? 1290 (list (make-student "John" 5903 empty) (make-student "Zaq" 1290 empty))) #false)
(check-expect (valid-id? 3957 (list S1 S2 S3)) #false)
(check-expect (valid-id? 1230 empty) #true)

; (4.)

;Signature: String Number String Number -> NULL
;Purpose: Adds a course to COURSES if the course doesn't exist 
(define (add-course dept course# teacher capacity)
  (if (valid-course? dept course# COURSES)
      (set! COURSES (cons (make-course dept course# teacher capacity empty) COURSES))
      (error "Course already exists!")))

;Signature: String Number ListOfCourse -> Boolean
;Purpose: Checks if a matching course exists, returns true if course is unique
(define (valid-course? dept course# a-loc)
  (cond
    [(empty? a-loc) #true]
    [(cons? a-loc) (if (and (string=? (course-dept (first a-loc)) dept) (= (course-course-num (first a-loc)) course#))
                       #false
                       (valid-course? dept course# (rest a-loc)))]))

(check-expect (valid-course? "MA" 1023 COURSES) #true)
(check-expect (valid-course? "ECE" 1010 (list C1 C2 C3)) #true)
(check-expect (valid-course? "CS" 2102 (list (make-course "PH" 1130 "Burnham" 255 empty) (make-course "CS" 2102 "Engling" 350 empty))) #false)

; (5.)

;Signature: Number String Number -> NULL
;Purpose: Adds the given student to a course and the respective course to the student if: the course exists, the student exists, and the course is not full
(define (add-to-course ID dept course#)
  (cond
    [(valid-id? ID STUDENTS) (error "Student does not exist!")]
    [(valid-course? dept course# COURSES) (error "Course does not exist!")]
    [else
     (if (< (length (course-a-los (get-course dept course# COURSES))) (course-max-enroll (get-course dept course# COURSES)))
         (begin
           (set-course-a-los! (get-course dept course# COURSES) (cons (get-student ID STUDENTS) (course-a-los (get-course dept course# COURSES))))
           (set-student-a-loc! (get-student ID STUDENTS) (cons (get-course dept course# COURSES) (student-a-loc (get-student ID STUDENTS)))))
         (error "Course is full!"))]))

;Signature: String Number ListOfCourse -> Course
;Purpose: Returns a course from a list of courses
(define (get-course dept course# a-loc)
  (cond
    [(empty? a-loc) empty]
    [(cons? a-loc) (if (and (string=? (course-dept (first a-loc)) dept) (= (course-course-num (first a-loc)) course#))
                         (first a-loc)
                         (get-course dept course# (rest a-loc)))]))

(check-expect (get-course "MA" 1024 CL1) C2)
(check-expect (get-course "CS" 1101 CL1) C1)
(check-expect (get-course "CHEM" 1020 CL1) empty)

;Signature: Number ListOfStudent -> Student
;Purpose: Returns a student from a list of students
(define (get-student ID a-los)
  (cond
    [(empty? a-los) empty]
    [(cons? a-los) (if (= (student-id (first a-los)) ID)
                          (first a-los)
                          (get-student ID (rest a-los)))]))

(check-expect (get-student 3957 SL1) S2)
(check-expect (get-student 1123 SL1) S1)
(check-expect (get-student 1385 STUDENTS) empty)

; (6.)

;Signature: NULL -> Course
;Purpose: Returns the course with the largest enrollment in COURSES
(define (largest-enrollment)
  (cond [(empty? COURSES) (error "No courses exist!")]
        [(cons? COURSES) (max-enroll (rest COURSES) (first COURSES))]))

;Signature: ListOfCourse Course -> Course
;Purpose: Returns the largest course in a list of courses
(define (max-enroll a-loc max)
  (cond [(empty? a-loc) max]
        [(cons? a-loc) (if (> (length (course-a-los (first a-loc))) (length (course-a-los max)))
                           (max-enroll (rest a-loc) (first a-loc))
                           (max-enroll (rest a-loc) max))]))
