(import (rnrs) (utility eval))

(display (eval-exp '1))

(display "\n\n")
(display (eval-exp '(lambda (x) x)))

(display "\n\n")
(display (eval-exp '((lambda (x) x) 
		     1)))

(display "\n\n")
(display (eval-exp '((lambda (x)
		       ((lambda (y) x)
			2))
		     1)))

(display "\n\n")
(display (eval-exp '((lambda (x)
		       ((lambda (f)
			  ((lambda (x)
			     (f 3))
			   2))
			(lambda (z) x)))
		     1)))
