#lang racket
; 1.a Function that creates a list with repeated values:
(define (replicate n c)
  (cond
   [ ( > n 0) (cons c (replicate (- n 1) c) )]
   [else '()]
   )
  )

; AC ==> '(a a a a a a a a a a)
(replicate 10 'a) 

; 1.b splits a list into pair of prefix of given size and remaining.
( define (split lst n)
   (cond
   [ (or (= n 0) (equal? (length lst) 0) )
     (cons '() (list lst) ) ]
   [ (>= n 0) (cons ( cons (first lst)
                          (car (split (rest lst) (- n 1)) ) )
                    (cdr (split (rest lst) (- n 1)) ) ) ]
   [else ('() lst) ]
   )
   )
; AC ==> '((1 2) . (3 4 5))
(split '(1 2 3 4 5) 2) 
; AC ==> '((a b c d) . ())
(split '(a b c d) 4)
; AC ==> '((a b c) . ())
(split '(a b c) 4)
; AC ==> '(() . (a b c))
(split '(a b c) 0)

; 1.c divide list into chunks of given size. 
(define (chunks lst n)
  (define (helper lst cur n)
  (cond
    [(equal? 0 (length lst) ) (list '())]
    [(equal? cur 0) (cons '() (helper lst n n))]
    [else (define new (helper (rest lst) (- cur 1) n) )
          (cons ( cons (car lst) (car new) )
                          (rest new) )])
    )
  (helper lst n n)
 )

; AC ==> '((1 2) (3 4) (5))
(chunks '(1 2 3 4 5) 2)
; AC ==> '((a b c) (d e f))
(chunks '(a b c d e f) 3)

; 1.d get list of sliding windows of given size.
(define (windows lst n)
  (define (helper lst n result)
    (cond
      [(equal? 0 (length lst)) result]
      [(equal? (length (first result)) n) 
       (helper (rest lst) n
               (cons (append (rest (first result) ) (list (first lst)) ) result) ) ]
      [else (helper (rest lst) n
                    (list (append (first result) (list (first lst) ) ) ))]
      ))
  (reverse (helper lst n (list '() ) ))
  )
; AC ==> '((1 2) (2 3) (3 4) (4 5))
(windows '(1 2 3 4 5) 2)
; AC ==> '((a b c) (b c d) (c d e))
(windows '(a b c d e) 3)

; 2.a generate list of all possible pairs in a list.
(define (pairs lst)
  (first (foldl
        (lambda (x y)
          (cond
            [(empty? (rest y)) y]
            [else (cons  (append (first y)
                         (map (lambda (z) (cons x z)) (rest y)))
                         (rest (rest y)) )]))
        (cons '() (rest lst)) lst)))

; AC ==> '((a . b) (a . c) (a . d) (b . c) (b . d) (c . d))
(pairs '(a b c d))

; 2.b generate all possible splits of a given list
(define (splits lst)
  (map (lambda (n) (split lst n)) (build-list (+ (length lst) 1) values) )
  )
; AC ==> '(((a b c) . ()) ((a b) . (c)) ((a) . (b c)) (() . (b c)))
(splits '(a b c))

; 2.c find two elements that give the highest product.
(define (max-product lst)
  (define maxE (argmax values lst))
  (cons maxE (argmax values (remove maxE lst)))
  )
; AC ==> '(3 . 4)
(max-product '(1 2 3 4 3 2 1))

; 2.d finds two elements of the list that maximize a given binary function.

(define (binary-op op expr)
  (op (car expr) (cdr expr)) )

(define (max-binary-op op lst)
  (foldl
   (lambda (x y)
     (cond
       [(or (empty? y)(> (binary-op op x) (binary-op op y))) x]
       [else y]))
   '() (pairs lst)))

; AC ==> '(3 . 4)
(max-binary-op * '(1 2 3 4 3 2 1))
; AC ==> '(4 . 1)
(max-binary-op - '(1 2 3 4 3 2 1))

; 2.e generate list of all unordered combinations of n elements from a list.
(define (combination element lst)
  (append lst (map
               (lambda (x) (append x (list element)))
               lst)))

(define (combinations lst size)
  (filter (lambda (x) (= (length x) size))
          (foldl
           (lambda (x y) (combination x y))
           '(()) lst)))

; AC ==> ((a b c) (a b d) (a c d) (b c d))
(combinations '(a b c d) 3)


; 3.a find maximum value.
(define (max lst)
  (foldl
   (lambda (x mx)
   (cond
     [ (> x mx) x]
     [else mx]
     )
   )
   0 lst
  )
)
; AC ==> 6 
(max '(1 5 3 6 2 0))

; 3.b find second maximum value
(define (second-max lst)
  (cdr (foldl
        (lambda (x mx)
          (cond
            [ (> x (car mx)) (cons x (car mx)) ]
            [ (> x (cdr mx)) (cons (car mx) x) ]
            [else mx]
            )
          )
        '(0 0) lst)
       )
  )

; AC ==> 5
(second-max '(1 5 3 6 2 0))

; 3.c find top 3 maximum values.
; Get top 3 maximum values.
(define (max-3 lst)
 (foldl
       (lambda (x mx)
         (cond
           [(> x (first mx)) (list x (first mx) (second mx) ) ]
           [(> x (second mx)) (list (first mx) x (second mx)) ]
           [(> x (third mx)) (list (first mx) (second mx) x) ]
           [else mx]
           ))
       '(0 0 0) lst)
  )
(define (top-3 lst)
  (cond
    [(equal? (length lst) 0) '()]
    [(equal? (length lst) 1) (list (first (max-3 lst)) ) ]
    [(equal? (length lst) 2) (list (first (max-3 lst)) (second (max-3 lst) ) ) ]
    [else (max-3 lst)]
    )
  )
; AC ==> (5 6 8)
(top-3 '(5 3 6 2 8 1 0))
; AC ==> (2 1)
(top-3 '(1 2))


; 3.d group consecutive equal elements together.
(define (group lst)
  (foldl
   (lambda (curr state)
     (cond
       [(empty? (first state)) (list (cons curr '()))]
       [(equal? (first (first (reverse state))) curr)
        (append
         (reverse (rest (reverse state)))
         (list (reverse (append (list curr) (first (reverse state))))))]
       [else (append state (list (list curr)))]))
   '(()) lst)
  )

; AC ==> '((a) (b b) (c c c) (b) (a a))
(group '(a b b c c c b a a))

; 3.e list of cumilative sum of all prefixes.
(define (cumulative-sums lst)
  (car
   (foldl
   (lambda (curr state)
     (list
      (append (first state) (list (+ curr (second state))))
      (+ curr (second state))))
   '((0) 0) lst)
       )
  )
; AC ==> '(0 1 3 6 10 15)
(cumulative-sums '(1 2 3 4 5))
