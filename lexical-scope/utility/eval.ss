(library (utility eval)
	 (export eval-exp *env*)
	 (import (rnrs) (utility tool) (utility datatype))
	 
	 (define *env* `(,(create-frame)))

	 (define (eval-exp exp env)
	   (handle-decision-tree 
	    `((,is-symbol? ,eval-symbol)
	      (,is-self-eval-exp? ,eval-self-eval-exp)
	      (,is-list?
	       ((,is-lambda? ,eval-lambda)
		(,is-function-call-list? ,eval-function-call-list))))
	    exp env))

	 (define (is-symbol? exp env)
	   (display "is-symbol?\n")
	   (symbol? exp))

	 (define (eval-symbol exp env)
	   (display "eval-symbol\n")
	   (get-symbol-value env exp))

	 (define (is-self-eval-exp? exp env)
	   (display "is-self-eval-exp?\n")
	   (number? exp))

	 (define (eval-self-eval-exp exp env)
	   (display "eval-self-eval-exp\n")
	   exp)

	 (define (is-list? exp env)
	   (display "is-list?\n")
	   (list? exp))

	 (define (is-lambda? exp env)
	   (display "is-lambda?\n")
	   (eq? (car exp) 'lambda))

	 (define (eval-lambda exp env)
	   (display "eval-lambda\n")
	   (let ((param (caadr exp))
		 (body (caddr exp)))
	     (make-closure param body env)))

	 (define (is-function-call-list? exp env)
	   (display "is-function-call-list?\n")
	   #t)

	 (define (eval-function-call-list exp env)
	   (display "eval-function-call-list\n")
	   (let* ((closure (eval-exp (car exp) env))
		  (arg (eval-exp (cadr exp) env))

		  (body (closure-body closure))
		  (lexical-env (closure-env closure))
		  (param (closure-param closure))

		  (frame (create-frame)))

	     (extend-frame frame param arg)

	     (let ((executing-env (extend-env lexical-env frame)))
	       (eval-exp body executing-env))))
	 
)
