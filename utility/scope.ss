(library (utility scope)
	 (export extend-env
		 add-to-frame
		 make-closure
		 get-closure-body
		 get-closure-param
		 get-closure-env)
	 (import (rnrs))

	 (define (extend-env frame env)
	   (cons frame env))

	 (define (add-to-frame frame key value)
	   (cons (cons key value) frame))

	 (define (make-closure proc env)
	   (cons proc env))

	 (define (get-closure-body closure)
	   (let ((proc (car closure)))
	     (caddr proc)))

	 (define (get-closure-param closure)
	   (let ((proc (car closure)))
	     (cadr proc)))

	 (define (get-closure-env closure)
	   (cdr closure))
)
