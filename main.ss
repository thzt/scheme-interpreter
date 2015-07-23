(import (rnrs) (utility eval) (utility scope))

;(eval '(if #t 1 2))
;(display (eval 'id *env*))

(eval-exp '(define add 
	     (lambda (x)
	       (lambda (y) (+ x y))))
	  *env*)

(eval-exp '(define add1 (add 1))
	  *env*)

(display (eval-exp '(add1 2)
	  *env*))

;(display (get-symbol-value-from-env *env* '+))
