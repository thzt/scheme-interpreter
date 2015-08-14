(library (utility eval)
	 (export eval-exp *env* *cont*)
	 (import (rnrs) (utility tool) (utility datatype))
	 
	 (define *env* `(,(create-frame)))

	 (define *cont* (lambda (v)
			  (display v)))

	 (define (eval-exp exp env cont)
	   (handle-decision-tree 
	    `((,is-symbol? ,eval-symbol)
	      (,is-self-eval-exp? ,eval-self-eval-exp)
	      (,is-continuation? ,eval-continuation)
	      (,is-list?
	       ((,is-lambda? ,eval-lambda)
		(,is-call/cc? ,eval-call/cc)
		(,is-continuation-call? ,eval-continuation-call)
		(,is-function-call-list? ,eval-function-call-list))))
	    exp env cont))

	 (define (is-symbol? exp env cont)
	   (display "is-symbol?\n")
	   (cont (symbol? exp)))

	 (define (eval-symbol exp env cont)
	   (display "eval-symbol\n")
	   (cont (get-symbol-value env exp)))

	 (define (is-self-eval-exp? exp env cont)
	   (display "is-self-eval-exp?\n")
	   (cont (number? exp)))

	 (define (eval-self-eval-exp exp env cont)
	   (display "eval-self-eval-exp\n")
	   (cont exp))

	 (define (is-continuation? exp env cont)
	   (display "is-continuation?\n")
	   (cont (continuation? exp)))

	 (define (eval-continuation exp env cont)
	   (display "eval-continuation\n")
	   (cont exp))

	 (define (is-list? exp env cont)
	   (display "is-list?\n")
	   (cont (list? exp)))

	 (define (is-lambda? exp env cont)
	   (display "is-lambda?\n")
	   (cont (eq? (car exp) 'lambda)))

	 (define (eval-lambda exp env cont)
	   (display "eval-lambda\n")
	   (let ((param (caadr exp))
		 (body (caddr exp)))
	     (cont (make-closure param body env))))

	 (define (is-call/cc? exp env cont)
	   (display "is-call/cc?\n")
	   (cont (eq? (car exp) 'call/cc)))

	 (define (eval-call/cc exp env cont)
	   (display "eval-call/cc\n")
	   (let ((fn (cadr exp))
		 (continuation (make-continuation cont)))
	     (eval-function-call-list `(,fn ,continuation) env cont)))

	 (define (is-continuation-call? exp env cont)
	   (display "is-continuation-call?\n")
	   (eval-exp (car exp) env
		     (lambda (value)
		       (cont (continuation? value)))))

	 (define (eval-continuation-call exp env cont)
	   (display "eval-continuation-call\n")
	   (eval-exp (car exp) env
		     (lambda (continuation)
		       (let ((wrapped-cont (continuation-cont continuation)))
			 (eval-exp (cadr exp) env
				   (lambda (arg)
				     (wrapped-cont arg)))))))

	 (define (is-function-call-list? exp env cont)
	   (display "is-function-call-list?\n")
	   (cont #t))

	 (define (eval-function-call-list exp env cont)
	   (display "eval-function-call-list\n")
	   (eval-exp (car exp) env
		     (lambda (closure)
		       (eval-exp (cadr exp) env
				 (lambda (arg)
				   (let ((body (closure-body closure))
					 (lexical-env (closure-env closure))
					 (param (closure-param closure))

					 (frame (create-frame)))

				     (extend-frame frame param arg)
				     
				     (let ((executing-env (extend-env lexical-env frame)))
				       (eval-exp body executing-env cont))))))))
	 
)