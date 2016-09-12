#lang racket

;;; Student Name: Frankly Olin [change to your name]
;;;
;;; Check one:
;;; [ ] I completed this assignment without assistance or external resources.
;;; [x] I completed this assignment with assistance from Joey Maalouf (explained how car, cdr, etc. works only)
	; He told me about the ) typo in the instructions
;;;     and/or using these external resources: ___

;;; 1.  Create a calculator that takes one argument: a list that represents an expression.

(define (calculate x)
  (cond
  	[(number? x) x] ;Check to see if is at bottom of loop
  	[(boolean? x) x] ;Check for boolean, at botom of loop (added in step 4)

  	;Created in part 1 & 2 - creates recursive calculation
  	[(eq? (first x) 'ADD) (+ (calculate (cadr x)) (calculate (caddr x)))]
  	[(eq? (first x) 'SUB) (- (calculate (cadr x)) (calculate (caddr x)))]
  	[(eq? (first x) 'MUL) (* (calculate (cadr x)) (calculate (caddr x)))]
  	[(eq? (first x) 'DIV) (/ (calculate (cadr x)) (calculate (caddr x)))]

  	;Created part 3 - handles booleans once, then recursion on the calculator
  	[(eq? (first x) 'GT) (> (calculate (cadr x)) (calculate (caddr x)))]
  	[(eq? (first x) 'LT) (< (calculate (cadr x)) (calculate (caddr x)))]
  	[(eq? (first x) 'GE) (>= (calculate (cadr x)) (calculate (caddr x)))]
  	[(eq? (first x) 'LE) (>= (calculate (cadr x)) (calculate (caddr x)))]
  	[(eq? (first x) 'EQ) (= (calculate (cadr x)) (calculate (caddr x)))]
  	[(eq? (first x) 'NEQ) (not (= (calculate (cadr x)) (calculate (caddr x))))]

  	;created part 4 - adds 3 new boolean operations
  	[(eq? (first x) 'ANND) (and (calculate (cadr x)) (calculate (caddr x)))]
  	[(eq? (first x) 'ORR) (or (calculate (cadr x)) (calculate (caddr x)))]
  	[(eq? (first x) 'NOTT) (not (calculate (cadr x)))]

  	;created part 5 - add if clause
  	[(eq? (first x) 'IPH) 
  		(if (calculate (cadr x)) 
  			(calculate (caddr x))
  			(calculate (cadddr x))
  			)]

  	;create part 6 - Unary minus
	[(eq? (first x) 'UM) (- (calculate (cadr x)))]

  	))


(calculate '(ADD 3 4)) ;; --> 7

;;; 2. Expand the calculator's operation to allow for arguments that are themselves well-formed arithmetic expressions.

(calculate '(ADD 3 (MUL 4 5))) ;; --> 23   ;; what is the equivalent construction using list?
(calculate '(SUB (ADD 3 4) (MUL 5 6))) ;; --> -23

;;; 3. Add comparators returning booleans (*e.g.*, greater than, less than, …).
;; Note that each of these takes numeric arguments (or expressions that evaluate to produce numeric values),
;; but returns a boolean.  We suggest operators `GT`, `LT`, `GE`, `LE`, `EQ`, `NEQ`.

	(calculate '(GT (ADD 3 4) (MUL 5 6))) ;; --> #f
	(calculate '(LE (ADD 3 (MUL 4 5)) (SUB 0 (SUB (ADD 3 4) (MUL 5 6))))) ;;G-> #t

;;; 4. Add boolean operations ANND, ORR, NOTT

(calculate '(ANND (GT (ADD 3 4) (MUL 5 6)) (LE (ADD 3 (MUL 4 5)) (SUB 0 (SUB (ADD 3 4) (MUL 5 6)))))) ;; --> #f​
; Here, I'm assuming it should be ANND, since AND isn't something we've added to the calculator

;;; 5. Add IPH

(calculate '(IPH (GT (ADD 3 4) 7) (ADD 1 2) (ADD 1 3))) ;; -> 4

;; 6. Add Unary Minus (apparently, scheme '- handles it automatically!)
(calculate '(ADD (UM 8) 6)) ;; -> -2