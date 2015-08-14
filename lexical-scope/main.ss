(import (rnrs) (utility eval))

(display (eval-exp '1 *env*))

(display "\n\n")
(display (eval-exp '(lambda (x) x) 
		   *env*))

(display "\n\n")
(display (eval-exp '((lambda (x) x) 
		     1) 
		   *env*))

(display "\n\n")
(display (eval-exp '((lambda (x)
		       ((lambda (y) x)
			2))
		     1) 
		   *env*))

(display "\n\n")
(display (eval-exp '((lambda (x)
		       ((lambda (f)
			  ((lambda (x)
			     (f 3))
			   2))
			(lambda (z) x)))
		     1)
		   *env*))
