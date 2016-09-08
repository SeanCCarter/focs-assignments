#lang racket

;;; Student Name: Sean Carter
;;;
;;; Check one:
;;; [ ] I completed this assignment without assistance or external resources.
;;; [x] I completed this assignment with assistance from ___
;;;     and/or using these external resources: https://docs.racket-lang.org/guide/index.html

;;     NOTE:  You need not worry about error checking in the programs below.

;; 1.  WRITE SQUARE:  given n, returns n^2.  Hint:  use *
(define (square x)
  (* x x))
  
(display (square 2)) (newline)  ;; -> 4
;;(newline)

;; 2.  WRITE is-right-triangle:  given three numbers, returns true iff the third
;;     could be the hypotenuse of a right triangle with the specified three side lengths
;;     Hint:  use = to compare numeric values

(define (is-right-triangle a b c)
  (= (square c) (+ (square a) (square b)) ))
  
(display (is-right-triangle 3 4 5)) (newline)  ;; -> #t
(display (is-right-triangle 4 5 6)) (newline)  ;; -> #f
;; (newline)

;; 3.  WRITE FACTORIAL:  given n, returns n!
;;     Hint:  recursion is your friend
(define (fact n)
  (if (= n 1) 
  	1 
  	(* n (fact (- n 1)))) )

(display (fact 1)) (newline) ;; -> 1
(display (fact 2)) (newline) ;; -> 1 ;;Student note: Pretty sure this is 2.
;; (newline)

;; 4.  WRITE FIBONACCI:  given n, returns the nth fibonacci number as shown below
;;     Hint:  don't run this on really big numbers!
(define (fib n)
   (cond [(= n 1) 1]
        [(= n 2) 1]
        [else (+ (fib (- n 1)) (fib (- n 2)))]))

(display (fib 1)) (newline) ;; -> 1
(display (fib 2)) (newline) ;; -> 1
(display (fib 3)) (newline) ;; -> 2
(display (fib 4)) (newline) ;; -> 3
(display (fib 5)) (newline) ;; -> 5
(display (fib 10)) (newline) ;; -> 8
;; (newline)

; 5.  WRITE a procedure that takes a list of numbers and returns the sum of those numbers
;     Hint:  first, rest, cons
(define (sum lst)
	(if [empty? lst] 0 (+ (first lst) (sum (rest lst))))
  )

(display (sum '(1 2 3 4))) (newline) ;; -> 10
(display (sum '(1 20 300))) (newline) ;; -> 321
;; (newline)

;; 6.  WRITE a procedure that takes a list of numbers and returns the largest one.
;;     While there are solutions using scheme's built-in max, we were actually hoping you'd do something else...
(define (my-max lst)
	(if [= (length lst) 1] 
		(first lst) 
		(if [> (first lst) (my-max (rest lst))] 
			(first lst) 
			(my-max (rest lst))) )
  
  )

(display (my-max '(1 10 2 20 3))) (newline) ;; -> 20
