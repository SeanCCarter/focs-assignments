#lang racket

(define (list-length xs)
	(if (empty? xs) 0 (+ 1 (list-length (rest xs))))
)

(display (list-length '()))(newline) ;0
(display (list-length '(a)))(newline) ;1
(display (list-length '(a b)))(newline) ;2
(display (list-length '(a b a b)))(newline) ;4
(newline)


(define (count-evens xs)
	(list-length (filter even? xs))
)

(display (count-evens '()))(newline) ;0
(display (count-evens '(1)))(newline) ;1
(display (count-evens '(1 2 3)))(newline) ;1
(display (count-evens '(1 4 9 10 12)))(newline) ;3
(newline)


(define (list-square xs)
	(map (lambda (x) (* x x)) xs)
)

(display (list-square '(1 2 3))) (newline) ;0
(newline)


(define (list-add1 xs)
	(map (lambda (x) (+ 1 x)) xs)
)

(display (list-add1 '(1 2 3))) (newline) ;0
(newline)


(define (list-add10 lst)
	(map (lambda (x) (+ 10 x)) lst)
)
(display (list-add10 '(1 2 3))) (newline) ;0
(newline)


(define (list-addn n lst)
	(map (lambda (x) (+ n x)) lst)
)

(display (list-addn 10 '(1 2 3))) (newline) ;0
(newline)

(define (filter-even lst)
	(if (empty? lst)
		null
		(if (even? (first lst)) 
			(cons (first lst) (filter-even (rest lst)))
			(filter-even (rest lst)))
	)
)

(display (filter-even '(1 2 3 5 8 13 21 34)))(newline)
(newline)

(define (filter-positive lst)
	(if (empty? lst)
		null
		(if (> (first lst) 0) 
			(cons (first lst) (filter-positive (rest lst)))
			(filter-positive (rest lst)))
	)
)

(display (filter-positive '(1 2 -3 -4 5 -6 7)))(newline)
(newline)

(define (filter-gt lower lst)
	(if (empty? lst)
		null
		(if (>= (first lst) lower) 
			(cons (first lst) (filter-gt lower (rest lst)))
			(filter-gt lower (rest lst)))
	)
)

(display (filter-gt 3 '(1 2 -3 -4 5 -6 7)))(newline)
(newline)
