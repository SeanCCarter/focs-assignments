#lang racket

(define (tree-add1 tree)
	(if [empty? tree] 
		empty?
		(list (+ 1 (first tree)) (tree-add1 (rest tree)))
	)
)

(display (tree-add1 '(3 ( 1 4)))) (newline) ;; -> 3