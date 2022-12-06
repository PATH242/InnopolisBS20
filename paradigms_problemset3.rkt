#lang racket
; 1.a convert binary string to decimal
( define (binary-to-decimal bits) (
          foldl (lambda (bit result) 
                (+ bit (* 2 result))
                )
                
   0
   bits
   )
)
; AC ==> 22
(displayln (binary-to-decimal'(1 0 1 1 0)) )

; 1.b remove leading zeros from a binary string
( define (remove-leading-zeros bits)
   (reverse
   (foldl (lambda(bit prev)
            (cond
             [ (and (= bit 0) (empty? prev) ) prev]
             [ else (cons bit prev)]
             )
            )
            '()
            bits
    ) )
) 
; AC ==> (1 0 1 1 0)
(displayln (remove-leading-zeros '(0 0 0 1 0 1 1 0))  )

; 1.c count zeroes in a binary string, not counting leading ones
( define (count-zeros bits)
   (foldl
    (lambda (bit bitCount)
           (cond
             [(= bit 0) (+ bitCount 1) ]
             [else bitCount]
             )
           )
    0
    (remove-leading-zeros bits)
    )
   )
; AC ==> 2
(displayln (count-zeros '(0 0 0 1 0 1 1 0)) )

; 1.d Group consecutive digits into lists.
( define (group-consecutive digits)
   ( reverse (foldl
    (lambda (bit lists)
      (cond
        [(empty? lists) (cons (cons bit '()) lists)]
        [(= bit (first (first lists) ) ) (cons (cons bit (first lists)) (rest lists) ) ]
        [else (cons (cons bit '()) lists)]
        )
      )
    '()
    digits)
             )
   )
; AC ==> '((0 0 0) (1) (0) (1 1) (0))
(displayln (group-consecutive '(0 0 0 1 0 1 1 0)) )

; 1.e Encode binary string by removing leading zeroes and
; replacing consecutive same digits with their count.
(define (encode-with-lengths bits)
  (reverse (foldl
   (lambda (digits result)
    (cons (length digits) result) )
   '()
   (group-consecutive (remove-leading-zeros bits))
   )
  )
  )

; AC ==> '(2 1 3 2)
(displayln (encode-with-lengths '(0 0 0 1 1 0 1 1 1 0 0)) )
 
; 1.f Decode binary string by reversing the previous exercise.
( define (decode-with-lengths lengths)
   (cdr
    (foldl
    (lambda (n result) 
    (cond
      [ (= (car result) 1) (cons 0 (append (cdr result) (make-list  n 1) ) )  ]
      [ else (cons 1 (append (cdr result) (make-list  n 0 ) ) ) ]
      )
      )
    '(1.  )
    lengths)
    )
   )
; AC ==> '(1 1 0 1 1 1 0 0)
(displayln (decode-with-lengths '(2 1 3 2)) )

; 2
;Pair of First name and (pair of last name and age).
(define employees
'(("John" "Malkovich" . 29)
("Anna" "Petrova" . 22)
("Ivan" "Ivanov" . 23)
("Anna" "Karenina" . 40)))

; 2.a get the full name of employee as a pair of (first. last)
(define (fullname employee)
  (cons (car employee) (car (cdr employee) ) )
  )
; AC ==> '("John" . "Malkovich")
(fullname '("John" "Malkovich" . 29))

; 2.b write an expression to get list of employees whose first name is anna
; expression is: (filter
; (lambda (employee) (equal? "Anna" (car employee)) )
; employees)

; AC ==> '( ("Anna" "Petrova" . 22) ("Anna" "Karenina" . 40)))
(displayln (filter
            (lambda (employee) (equal? "Anna" (car employee)) )
            employees) )

; 2.c get list of employees whose age is >25
(define (employees-over-25 employees)
  (filter (lambda (employee)
            ( < 25 (cdr (cdr employee)) ) )
          employees)
  )

; AC ==> '(("John" . "Malkovich") ("Anna" . "Karenina"))
(employees-over-25 employees)




