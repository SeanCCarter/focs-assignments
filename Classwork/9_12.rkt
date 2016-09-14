#lang racket

;; Question 1
; Recursive factorial function
(define (factorial-recursive n)
	(if (= n 1) 1 (* n (factorial-recursive (- n 1)))) 
)


; tail-recursive fatorial function
(define (factorial-tail n)
	(fact n 1)
)

(define (fact n so-far)
	(if (= n 1) so-far (fact (- n 1) (* n so-far))) 
)

(factorial-recursive 5)
(factorial-tail 5)
;;-----------------------------------------------------

;; Question 2
(define (is_even value)
	(if (= (remainder value 2) 0) (list value) '()))

(define (is_teen value)
	(if (and (>= value 13) (<= value 19)) (list value) '()))

(define (is_list value)
	(if (list? value) (list value) '()))

(define (my-filter fltr lst)
	(cond 
		[(null? lst) '()]
		[(eq? fltr 'EVEN) (append (is_even (first lst)) (my-filter fltr (rest lst)))]
		[(eq? fltr 'TEEN) (append (is_teen (first lst)) (my-filter fltr (rest lst)))]
		[(eq? fltr 'LIST) (append (is_list (first lst)) (my-filter fltr (rest lst)))]
	)
)

(my-filter 'EVEN '(1 2 3 4 5 6))
(my-filter 'TEEN '(21 17 2 13 4 42 2 16 3))
(my-filter 'LIST '(3 (3 2 1) symbol (4 2) (1 (2) 3)))

;Takes type of filter, and a list
;--------------------------------------------------------

;;Question 3

(define (double x) (* 2 x))

(define (my-map function lst)
	(if (empty? lst) 
		'() 
		(cons (function (first lst)) (cons(my-map function lst)))))

(my-map double '(1 2 3))