(import (rnrs) (utility eval))

(display (eval-exp '(define-syntax insert-binding
		      (lambda (x)
			((lambda (a)
			   x)
			 1)))
		   *env*))

(display (eval-exp '((lambda (a)
		       (insert-binding a))
		     2)
		   *env*))

