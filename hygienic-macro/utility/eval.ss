(library (utility eval)
	 (export eval-exp *env*)
	 (import (rnrs) (utility tool) (utility datatype))
	 
	 (define *env* `(,(create-frame)))

	 (define (eval-exp exp env)
	   (handle-decision-tree 
	    `((,is-macro-binding? ,eval-macro-binding)
	      (,is-symbol? ,eval-symbol)
	      (,is-self-eval-exp? ,eval-self-eval-exp)
	      (,is-list?
	       ((,is-lambda? ,eval-lambda)
		(,is-define-syntax? ,eval-define-syntax)
		(,is-macro-call? ,eval-macro-call)
		(,is-function-call-list? ,eval-function-call-list))))
	    exp env))

	 (define (is-macro-binding? exp env)
	   (display "is-macro-binding?\n")
	   (if (symbol? exp)
	       (let ((value (get-symbol-value env exp)))
		 (macro-binding? value))
	       #f))

	 (define (eval-macro-binding exp env)
	   (display "eval-macro-binding\n")
	   (let* ((value (get-symbol-value env exp))
		  (binding-value (macro-binding-value value))
		  (eval-env (macro-binding-eval-env value)))
	     (eval-exp binding-value eval-env)))

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

	 (define (is-macro-call? exp env)
	   (display "is-macro-call?\n")
	   (macro? (eval-exp (car exp) env)))

	 (define (eval-macro-call exp env)
	   (display "eval-macro-call\n")
	   (let* ((arg (cadr exp))

		  (macro (eval-exp (car exp) env))
		  (param (macro-param macro))
		  (body (macro-body macro))
		  (lexical-env (macro-env macro))
		  
		  (frame (create-frame)))

	     (extend-frame frame param 
			   (make-macro-binding arg env))

	     (let ((executing-env (extend-env lexical-env frame)))
	       (eval-exp body executing-env))))

	 (define (is-define-syntax? exp env)
	   (display "is-define-syntax?\n")
	   (eq? (car exp) 'define-syntax))

	 (define (eval-define-syntax exp env)
	   (display "eval-define-syntax\n")
	   (let* ((param (caadr (caddr exp)))
		  (body (caddr (caddr exp)))

		  (macro-name (cadr exp))
		  (macro (make-macro param body env)))

	     (modify-env env macro-name macro)))
		 
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
