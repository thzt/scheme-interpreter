(library (utility eval)
	 (export eval-exp)
	 (import (rnrs) (utility tool) (utility datatype))

	 (define (eval-exp exp)
	   (handle-decision-tree 
	    `((,is-symbol? ,eval-symbol)
	      (,is-self-eval-exp? ,eval-self-eval-exp)
	      (,is-list?
	       ((,is-lambda? ,eval-lambda)
		(,is-function-call-list? ,eval-function-call-list))))
	    exp))
	 
	 (define *env* `(,(create-frame)))

	 (define (is-symbol? exp)
	   (display "is-symbol?\n")
	   (symbol? exp))

	 (define (eval-symbol exp)
	   (display "eval-symbol\n")
	   (get-symbol-value *env* exp))

	 (define (is-self-eval-exp? exp)
	   (display "is-self-eval-exp?\n")
	   (number? exp))

	 (define (eval-self-eval-exp exp)
	   (display "eval-self-eval-exp\n")
	   exp)

	 (define (is-list? exp)
	   (display "is-list?\n")
	   (list? exp))

	 (define (is-lambda? exp)
	   (display "is-lambda?\n")
	   (eq? (car exp) 'lambda))

	 (define (eval-lambda exp)
	   (display "eval-lambda\n")
	   (let ((param (caadr exp))
		 (body (caddr exp)))
	     (make-function param body)))

	 (define (is-function-call-list? exp)
	   (display "is-function-call-list?\n")
	   #t)

	 (define (eval-function-call-list exp)
	   (display "eval-function-call-list\n")
	   (let* ((function (eval-exp (car exp)))
		  (arg (eval-exp (cadr exp)))

		  (body (function-body function))
		  (param (function-param function))

		  (frame (create-frame)))

	     (extend-frame frame param arg)

	     (let* ((env *env*)
		    (value '()))
	       (set! *env* (extend-env *env* frame))
	       (set! value (eval-exp body))
	       (set! *env* env)
	       value)))
)
