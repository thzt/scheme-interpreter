(import (rnrs) (utility eval))

(eval-exp '1 *env* *cont*)

(display "\n\n")
(eval-exp '(lambda (x) x) 
		   *env* *cont*)

(display "\n\n")
(eval-exp '((lambda (x) x) 
		     1) 
		   *env* *cont*)

(display "\n\n")
(eval-exp '((lambda (x)
		       ((lambda (y) x)
			2))
		     1) 
		   *env* *cont*)

(display "\n\n")
(eval-exp '((lambda (x)
		       ((lambda (f)
			  ((lambda (x)
			     (f 3))
			   2))
			(lambda (z) x)))
		     1)
		   *env* *cont*)
