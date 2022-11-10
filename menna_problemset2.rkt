#lang racket
; 1.a convert binary string to decimal
( define (binary-to-decimal lst)
   ( define (helper lst power)
      (cond [(empty? lst) 0]
      [else (+  ( * (first lst) (expt 2 power) ) (helper (rest lst) (- power 1)))]
      ) )
   (helper lst (- (length lst) 1))
   )
; AC ==> 22
(displayln (binary-to-decimal'(1 0 1 1 0)) )

; 1.b count zeroes in a binary string, ignoring leading zeroes
( define (count-zeros lst)
   (define (helper lst foundOne)
     (cond [ (empty? lst) 0]
           [else     (cond [ (and (= foundOne 1) ( = (first lst) 0)) (+ 1 (helper (rest lst) (cond [ (or (= foundOne 1) (= (first lst) 1)) 1][else 0] ) ))]
           [else  (helper (rest lst) (cond [ (or (= foundOne 1) (= (first lst) 1)) 1][else 0] ) ) ]) ]
     )
     )
     (helper lst 0)
     )
; AC ==> 2
(displayln (count-zeros '(0 0 0 1 0 1 1 0)) )

; 1.c encode with lengths and remove leading zeroes
( define (encode-with-lengths lst)
   ( define (helper lst last count)
      (cond [ (empty? lst) (list count) ]
            [ (= (first lst) last) (helper (rest lst) last (+ 1 count) ) ]
            [ else (cons count (helper (rest lst) (first lst) 1) ) ]
            )
      )
   (rest (helper lst 0 0) )
   )

; AC ==> '(2 1 3 2)
(displayln (encode-with-lengths '(0 0 0 1 1 0 1 1 1 0 0)) )

;1.d is the binary string an odd number?
( define (binary-odd lst)
   (cond [ (empty? lst) #f]
     [ (= (length lst) 1) (cond [ (= (first lst) 1) #t] [else #f])]
     [else (binary-odd (rest lst))]
     )
   )
(displayln (binary-odd '(1 0 1 1 0)) )
(displayln (binary-odd '(1 0 1 1 1)) )

;1.e decrement the binary string by 1

; utility function to remove extra preceding zeroes
( define ( removePrecedingZeroes lst)
   (cond [ (empty? lst) null ]
         [ (= (length lst) 1) lst]
         [ ( = (first lst) 1) lst]
         [ (removePrecedingZeroes (rest lst)) ]
         )
   )
(define (decrement lst)
  (define (helper lst lastOneFound)
  (cond [ (empty? lst) '() ]
        [ (= 1 lastOneFound) (cons 1 (helper (rest lst) lastOneFound) ) ]
        [ (= (count-zeros lst) ( - (length lst) 1) ) (cons 0 (helper (rest lst) 1) ) ]
        [else (cons (first lst) (helper (rest lst) 0) ) ] 
        )
    )
  ( removePrecedingZeroes (helper lst 0) )
  )
; AC ==> '(1 0 1 0 1)
(displayln (decrement '(1 0 1 1 0)) )
; AC ==> '(1 1 1 1)
(displayln (decrement '(1 0 0 0 0)) )
 ; AC ==> '(0)
(decrement '(0))

; 2.a Compute a - b + c - d ...etc
 ( define (alternating-sum lst)
    ( cond [ (empty? lst) 0]
           [ (= 1 (length lst) ) (first lst) ]
           [ ( + ( + (first lst) ( - 0 ( first (rest lst) ) ) )  (alternating-sum (rest (rest lst))) ) ]
           )
    )
; AC  ==> 3
(displayln (alternating-sum (list 1 2 3 4 5)) )

; 2.b
; Substitution model:
; (alternating-sum (list 1 2 3 4 5))
; (alternating-sum '(1 2 3 4 5))
; (+ (+ 1 -2 ) (alternating-sum '(3 4 5)))
; (+ (+ 1 -2 ) (+ (+ 3 -4) (alternating-sum '(5))))
; (+ (+ 1 -2 ) (+ (+ 3 -4) 5))
; (+ -1 (+ -1 5))
; (+ -1 4)
; 3

; 2.c
; Using tail recursion is better for this problem indeed since it will perform the recursion last,
; return the desired value directly after computation, and save us adding stack frames.


; 3 make a substitution model for this code (f 3)

(define (dec n) (- n 1))
(define (f n)
(cond
[(<= n 2) (- 10 n)]
[else (* (f (dec (dec n))) (f (dec n)))]))

; AC ==> 72
(displayln  (f 3) )
; Substitution model:
; (f 3)
; (* (f (dec (dec n))) (f (dec n)))
; (* (f (dec (dec 3))) (f (dec 3)))
; (* (f (dec 2)) (f 2))
; (* (f 1) (f 2))
; (* 9 8)
; (* 9 8)
; 72
