(import (rnrs) (utility eval) (utility scope))

(eval-exp '(if #t 1 2) *env* *cont*)

(eval-exp '(+ 1 2) *env* *cont*)

(eval-exp '(define a 1) *env* *cont*)

(eval-exp '(define a (lambda (x) x)) *env* *cont*)
(eval-exp '(a 1) *env* *cont*)

(eval-exp '(define add1 (lambda (x) (+ x 1))) *env* *cont*)
(eval-exp '(add1 2) *env* *cont*)

(eval-exp '(call/cc (lambda (k) 1))
	  *env* *cont*)
(eval-exp '(call/cc (lambda (k) (k 2)))
	  *env* *cont*)

