#lang racket

;;; Student Name: Frankly Olin [change to your name]
;;;
;;; Check one:
;;; [ ] I completed this assignment without assistance or external resources.
;;; [ ] I completed this assignment with assistance from ___
;;;     and/or using these external resources: ___
(define (assq key lst)
	(if (empty? lst) 
		#f
	 	(if (eq? key (caar lst)) 
	 		(first lst) 
	 		(assq key (rest lst))
	 	)
	)
)

(define (calculate x lookup-list)
  (cond
  	[(number? x) x] ;Check to see if is at bottom of loop
  	[(boolean? x) x] ;Check for boolean, at botom of loop (added in step 4)
  	[(symbol? x) (cadr (assq x lookup-list))]

  	;Create a var or lambda
  	[(eq? (first x) 'DEFINE) (update_environment (cadr x) (calculate (caddr x) lookup-list) lookup-list)]
  	[(eq? (first x) 'LAMBDA) (list 'lambda (cadr x) (caddr x) lookup-list)]

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

	;evaluate lanbda expressions
	[(eq? (caar x) 'lambda) (evaluate-lambda (car x) (rest x) lookup-list)]
	[(list? (calculate (first x) lookup-list)) (calculate (list (calculate (first x) lookup-list) (rest x)) lookup-list)]

	[else (error "You done messed up")]
 ))

 (define (evaluate-lambda function args lookup-list)
 	(calculate (caddr function) (append (cadddr function) (zip (cadr function) (evaluate-args (car args) lookup-list))))
 	)

(define (evaluate-args args lookup-list)
	(map (lambda (x) (calculate x lookup-list)) args))

(define (update_environment x value lookup-list)
	(repl (append lookup-list (list(list x value))))
	)

(define (zip lista listb)
	(if (or (empty? lista) (empty? listb))
		'()
		(cons (list (first lista) (first listb)) (zip (rest lista) (rest listb)))
	)
)


(define (run-repl environment)
  (display "welcome to my repl.  type some scheme-ish")
  (repl environment))

(define (repl environment)
  (display "> ")
  (display (calculate (read) environment))
  (newline)
  (repl environment))


(run-repl '())
;(zip (cadr '(lambda (a b) (ADD a b) ())) (list 1 2))