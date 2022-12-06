#lang racket
; The following code assumes that valid expressions
; will only contain +, *,^, (log x y), (ln x), (sin x), (cos x), (tan x) , numbers, and letters.

; Assignment 1.
; 1.1
; Valid variables.
(define variables
  '(a b c d e f g h i j k l
   m n o p q r s t u v w x y z
  A B C D E F G H I J K L M
     N O P Q R S T U V W X
     Y Z)
  )
; Check whether a given expression is a valid variable.
(define (variable? expr)
  (cond
    [(and (atomic? expr) (member expr variables) )#t]
    [else #f]
    )
)
; Check whether a given expression is a constant.
(define (constant? expr)
  (cond
    [(number? expr) #t]
    [else #f]
    )
  )
; Check whether a given expresion is atomic
( define (atomic? expr)
  (cond
   [(list? expr) #f]
   [else #t]
   ))


; Check whether a given expression is a sum.
(define (sum? expr)
  ( cond
     [(atomic? expr) #f]
     [ (and (equal? '+ (first expr)) (> (length expr) 2))
             (andmap valid-term? (rest expr))]
     [else #f]
     )
  )
; Check whether a given expression is a product.
(define (product? expr)
  ( cond
     [(atomic? expr) #f]
     [ (and (equal? '* (first expr)) (> (length expr) 2))
             (andmap valid-term? (rest expr))]
     [else #f]
     ) )

; Check whether a given expression is a valid trigonometric function.
(define (trig? expr)
  (cond
    [ (atomic? expr) #f]
    [(and (equal? (first expr) 'sin) (equal? 2 (length expr)) ) #t ]
    [(and (equal? (first expr) 'cos) (equal? 2 (length expr)) ) #t ]
    [(and (equal? (first expr) 'tan) (equal? 2 (length expr)) ) #t ]
    [else #f]
    )
  )
; Check whether a given expression is an exponential.
(define (exponent? expr)
  (cond
     [ (atomic? expr) #f]
     [(and (equal? (first expr) '^) (equal? (length expr) 3) ) #t ]
     [else #f]
    )
  )
; Check whether expression is log.
(define (log? expr)
  (cond
    [(atomic? expr) #f]
    [(and (equal?  (first expr) 'log ) (equal? (length expr) 3) ) #t]
    [(and (equal? 'ln (first expr)) (equal? (length expr) 2) ) #t]
    [else #f]
    )
  )
; Check whether a given expression is a valid term.
(define (valid-term? expr)
  (cond
    [ (or (constant? expr) (variable? expr) ) #t]
    [ ( or (sum? expr) (product? expr)) #t]
    [ (or (trig? expr) (exponent? expr)) #t]
    [ (log? expr) #t]
    [ else #f]
    )
  )

; Extract first summand from a sum expression.
(define (summand-1 expr)
  (second expr) )
; Extract second summand from a sum expression.
(define (summand-2 expr)
  (third expr) )

; Extract first multiplier from a product expression.
(define (multiplier-1 expr)
  (second expr) )
; Extract second multipler from a product expression.
(define (multiplier-2 expr)
  (third expr) )

; 1.2 + 1.6 + 1.7
; Get derivative of an expression.
(define (derivative expr var)
  (cond
    [ (sum? expr) (sumDerivative expr var)]
    [ (product? expr) (productDerivative expr var) ]
    [(trig? expr) (trigDerivative expr var)]
    [(exponent? expr) (exponentDerivative expr var)]
    [(log? expr) (logDerivative expr var) ]
    [ (and (atomic? expr) (equal? expr var)) 1]
    [else 0]
   )
  )
; Get derivative of a sum expression.
(define (sumDerivative expr var)
    (cons '+ (map (lambda (x) (derivative x var) )
                         (rest expr) ) ) 
 )
; Get derivative of a product expression.
(define (productDerivative expr var)
    (cons '+
          (map (lambda (x)
                 (cons '* (cons (derivative x var) (remove x (rest expr))) ) )
               (rest expr))
    )
 )
; Get derivative of a trignometric expression.
(define (trigDerivative expr var)
  (cond
    [(equal? (first expr) 'sin)
     (list '* (list 'cos (second expr)) (derivative (second expr) var) )]
    [(equal? (first expr) 'cos)
     (list '* (list 'sin (second expr)) (derivative (second expr) var ))]
    [(equal? (first expr) 'tan)
     (list '* ( list '^ (list 'sec (second expr)) 2) (derivative (second expr) var) )]
    )
  )

; Get derivative of an exponential expression.
(define (exponentDerivative expr var)
   [ list '*
           (list '^ (second expr) (third expr) )
           (list '+
                 (list '/
                 (list '* (third expr) (derivative (second expr) var) )
                 (second expr))
                (list '*
                      (derivative (third expr) var) (list 'ln (second expr) ))
                )
           ]
  )

; Get derivative of logarithm expression.
(define (logDerivative expr var)
  (cond
   [(equal? (first expr) 'log)
    (list '/ (derivative (third expr) var)
          (list '* (third expr) (list 'ln (second expr) ) ) )]
   [else (list '/ (derivative (second expr) var) (third expr) ) ]
 )
  )



; 1.3 simplify expressions + 1.6 + 1.7

; Helper function to simplify sum expressions.
(define (simplify-sum expr)
  (cond
                    [ (andmap constant? (rest expr)) (foldl + 0 (rest expr))]
                    [ else (define x (cons '+ (map simplify (rest expr))) )
                           (define xs (remove* (list 0) x) )
                           (cond [(equal? 2 (length xs) ) (second xs)]
                                 [else xs]) ] )
  )
; Helper function to simplify product expressions. 
(define (simplify-product expr)
  (cond
       [ (andmap constant? (rest expr)) (foldl * 1 (rest expr)) ]
       [ (ormap (lambda (x) (equal? x 0) ) (rest expr) ) 0]
       [ else (define x (cons '* (map simplify (rest expr)) ) )
              (define xs (remove* (list 1) x))
                 (cond [(equal? 2 (length xs)) (second xs)]
                       [else xs]) ] )
  )
; Helper function to simplify exponentials.
(define (simplify-exponent expr)
   (cond
       [ (and (constant? (multiplier-1 expr))
              (constant? (multiplier-2 expr)) )
         (expt (multiplier-1 expr) (multiplier-2 expr))]
       [ (equal? (multiplier-2 expr) 0) 1]
       [ (equal? (multiplier-2 expr) 1) (multiplier-1 expr)]
       [ else expr ] )
)
; Helper function to simplify trignometric expressions.
(define (simplify-trig expr)
  (cond
    [ (constant? (second expr))
      (cond
        [(equal? 'sin (first expr)) (sin (second expr))]
        [(equal? 'cos (first expr)) (cos (second expr))]
        [(equal? 'tan (first expr)) (tan (second expr))]) ]
    
  [else expr]
  )
  )
; Helper function to simplify log expressions.
(define (simplify-log expr)
  (cond
    [(equal? (first expr) 'log)
     (cond
          [(equal? (second expr) (third expr)) 1]
          [(equal? (third expr) 1) 0]
          [else expr]) ]
     [else (cond
                 [(equal? (second expr) 'e) 1]
                 [(equal? (second expr) 1) 0]
                 [else expr]) ]
  )
  )
; Simplify at root.
( define (simplify-at-root expr)
   (cond
     [(sum? expr) (simplify-sum expr)]
     [(product? expr) (simplify-product expr) ]
     [(exponent? expr) (simplify-exponent expr) ]
     [(trig? expr) (simplify-trig expr) ]
     [(log? expr) (simplify-log expr) ]
     [else expr ]
     )
   )
; Simplify expression.
(define (simplify expr)
  (cond
    [(atomic? expr) expr]
    [(sum? expr)
     (simplify-at-root (cons '+ (map simplify (rest expr) ))) ]
    [(product? expr)
     (simplify-at-root (cons '* (map simplify (rest expr) ) ) ) ]
    [(exponent? expr)
     (simplify-at-root (cons '^ (list (simplify (second expr)) (simplify (third expr)))) ) ]
    [(log? expr)
     (cond [
           (equal? (first expr) 'log)
           (simplify-at-root (cons 'log (list (simplify (second expr)) (simplify (third expr)))) )]
           [else  (simplify-at-root (cons 'ln (list (simplify (second expr)) )) ) ]
            )]
    [(trig? expr) (simplify-at-root (cons (first expr) (list (simplify (second expr) ) ) ) )]
    [else expr]
    )
  )


;1.5 convert expression to infix form.
(define (to-infix expr)
  (cond
    [ (sum? expr)
      (list (to-infix (summand-1 expr)) '+ (to-infix (summand-2 expr)) ) ]
    [(product? expr)
     (list (to-infix (multiplier-1 expr)) '* (to-infix (multiplier-2 expr)) ) ]
    [(exponent? expr)
     (list (to-infix (multiplier-1 expr)) '^ (to-infix (multiplier-2 expr)) ) ]
    [else expr]
    )
  )



; 1.8
; Get a sorted list of distinct variables used in expression.
(define (variables-of expr)
  (sort
   (remove-duplicates
    (append
     (filter variable? expr)
     (apply append (map variables-of (filter list? expr))))
     )
     #:key symbol->string string<?
   )
  )

; 1.9
; Get gradiant of a multivariable expression.
(define (gradient expr vars)
  (map (lambda (var) (simplify (derivative expr var) )) vars)
)
;-------------------------------------------------------------------------
; some testing
; AC ==> '(+ 0 1)
(derivative '(+ 1 x) 'x)
; AC ==> '(+ (* 0 y) (* 2 1))
(derivative '(* 2 y) 'y)
; AC ==> '(+ (* (+ 1 0) (+ x (+ x x))) (* (+ x y) (+ 1 (+ 1 1))))
(derivative '(* (+ x y) (+ x (+ x x))) 'x)
; AC ==>'(/ 1 (* x (ln 2)))
(derivative (list 'log 2 'x) 'x)

; AC ==> '(* (cos x) 1)
(derivative '(sin x) 'x)
; AC == > '(* (^ 2 x) (+ (/ (* x 0) 2) (* 1 (ln 2))))
(derivative '(^ 2 x) 'x)
; AC ==> (* (^ 2 x) (ln 2) )
(simplify  '(* (^ 2 x) (+ (* x 0) (* 1 (ln 2))) ) )
; AC ==> 1
(simplify '(+ 0 1))
; AC ==> 2
(simplify '(+ (* 0 y) (* 2 1)))

; AC ==> '(+ 0 1 0 (+ (* 1 y z) (* x 0 z) (* x y 0)))
(derivative '(+ 1 x y (* x y z)) 'x)
; AC ==> '(+ 1 (* y z))
(simplify '(+ 0 1 0 (+ (* 1 y z) (* x 0 z) (* x y 0))))
; AC ==> '(+ (+ x (+ x x)) (* (+ x y) 3))
(simplify '(+ (* (+ 1 0) (+ x (+ x x))) (* (+ x y) (+ 1 (+ 1 1)))))

; AC ==> '((x + (x + x)) + ((x + y) * 3)
(to-infix '(+ (+ x (+ x x)) (* (+ x y) 3)) )
; AC ==> '(x y z)
(variables-of '(+ 1 x y (* x y z)))
; AC ==> '((+ 1 (* y z)) (+ 1 (* x z)) (* x y))
(gradient '(+ 1 x y (* x y z)) '(x y z)) 