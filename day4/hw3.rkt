#lang racket

;;; Student Name: Sean Carter
;;;
;;; Check one:
;;; [ ] I completed this assignment without assistance or external resources.
;;; [ ] I completed this assignment with assistance from ___
;;;     and/or using these external resources: ___

;;;;;;;;;;;
;; 1. assq

;; `assq` is a function that takes a key and an association list.
;;
;; It returns the corresponding key/value pair from the list
;; (*i.e.*, the pair whose key is *eq?* to the one it is given).
;;
;; If the key is not found in the list, `assq` returns `#f`.
(define operator-list
  (list (list 'ADD +)
        (list 'SUB -)
        (list 'MUL *)
        (list 'DIV /)
        (list 'GT >)
        (list 'LT <)
        (list 'GE >=)
        (list 'LE <=)
        (list 'EQ =)
        (list 'NEQ (lambda (x y) (not (= x y))))
        (list 'ANND (lambda (x y) (and x y)))
        (list 'ORR (lambda (x y) (or x y)))
        (list 'NOTT not)))


(define (assq key lst)
	(if (empty? lst) 
		#f
	 	(if (eq? key (caar lst)) 
	 		(first lst) 
	 		(assq key (rest lst))
	 	)
	)
)

(assq 'ADD operator-list) ;; --> '(ADD #<procedure:+>)
(assq 'ANND operator-list) ;; --> '(ANND #<procedure>)
(assq 'FOO operator-list) ;; --> #f

;;;;;;;;;;;
;; 2. lookup-list

;; Add the ability to look up symbols to your evaluator.
;;
;; Add the `lookup-list` argument to your hw2 evaluator (or ours, from the solution set).
;; `(evaluate 'foo lookup-list)` should return whatever `'foo` is associated with in `lookup-list`.


(define (calculate x lookup-list)
  (cond
  	[(number? x) x] ;Check to see if is at bottom of loop
  	[(boolean? x) x] ;Check for boolean, at botom of loop (added in step 4)
  	[(symbol? x) (cadr (assq x lookup-list))]

  	;Created in part 1 & 2 - creates recursive calculation
  	[(eq? (first x) 'ADD) (+ (calculate (cadr x) lookup-list) (calculate (caddr x) lookup-list))]
  	[(eq? (first x) 'SUB) (- (calculate (cadr x) lookup-list) (calculate (caddr x) lookup-list))]
  	[(eq? (first x) 'MUL) (* (calculate (cadr x) lookup-list) (calculate (caddr x) lookup-list))]
  	[(eq? (first x) 'DIV) (/ (calculate (cadr x) lookup-list) (calculate (caddr x) lookup-list))]

  	;Created part 3 - handles booleans once, then recursion on the calculator
  	[(eq? (first x) 'GT) (> (calculate (cadr x) lookup-list) (calculate (caddr x) lookup-list))]
  	[(eq? (first x) 'LT) (< (calculate (cadr x) lookup-list) (calculate (caddr x) lookup-list))]
  	[(eq? (first x) 'GE) (>= (calculate (cadr x) lookup-list) (calculate (caddr x) lookup-list))]
  	[(eq? (first x) 'LE) (>= (calculate (cadr x) lookup-list) (calculate (caddr x) lookup-list))]
  	[(eq? (first x) 'EQ) (= (calculate (cadr x) lookup-list) (calculate (caddr x) lookup-list))]
  	[(eq? (first x) 'NEQ) (not (= (calculate (cadr x) lookup-list) (calculate (caddr x) lookup-list)))]

  	;created part 4 - adds 3 new boolean operations
  	[(eq? (first x) 'ANND) (and (calculate (cadr x) lookup-list) (calculate (caddr x) lookup-list))]
  	[(eq? (first x) 'ORR) (or (calculate (cadr x) lookup-list) (calculate (caddr x) lookup-list))]
  	[(eq? (first x) 'NOTT) (not (calculate (cadr x) lookup-list))]

  	;created part 5 - add if clause
  	[(eq? (first x) 'IPH) 
  		(if (calculate (cadr x) lookup-list) 
  			(calculate (caddr x) lookup-list)
  			(calculate (cadddr x) lookup-list)
  			)]

  	;create part 6 - Unary minus
	[(eq? (first x) 'UM) (- (calculate (cadr x) lookup-list))]

 ))

(define thingie
	'((x 3) (y 12) (z 2))
)

(calculate '(ADD x y) thingie)

