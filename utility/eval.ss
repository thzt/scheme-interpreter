(library (utility eval)
	 (export eval-exp *env*)
	 (import (rnrs) (utility tool) (utility scope))
	 
	 ;export

	 (define *env* `(,(create-frame)))

	 (define (eval-exp exp env)
	   (handle-tree `(
			  (,is-symbol? ,eval-symbol)
			  (,self-eval-exp? ,eval-self-eval-exp)
			  (,is-list?
			   ((,special-form-list?
			     ((,is-if? ,eval-if)
			      (,is-define? ,eval-define)
			      (,is-set!? ,eval-set!)
			      (,is-lambda? ,eval-lambda)
			      (,is-plus? ,eval-plus)
			      (,is-minus? ,eval-minus)))
			    (,function-call-list? ,eval-function-call-list))))
			exp env))

	 ;private region

	 (define (is-symbol? exp)
	   (display "is-symbol?\n")
	   (symbol? exp))

	 (define (eval-symbol exp env)
	   (display "eval-symbol\n")
	   (get-symbol-value-from-env env exp))

	 (define (self-eval-exp? exp)
	   (display "self-eval-exp?\n")
	   (or (number? exp) (boolean? exp)))

	 (define (eval-self-eval-exp exp env)
	   (display "eval-self-eval-exp\n")
	   exp)

	 (define (is-list? exp)
	   (display "is-list?\n")
	   (list? exp))

	 (define (special-form-list? exp)
	   (display "special-form-list?\n")
	   (member (car exp) '(if define set! lambda + -)))

	 (define (is-if? exp)
	   (display "if?\n")
	   (eq? (car exp) 'if))

	 (define (eval-if exp env)
	   (display "eval-if\n")
	   (if (eval-exp (cadr exp) env)
	       (eval-exp (caddr exp) env)
	       (eval-exp (cadddr exp) env)))

	 (define (is-define? exp)
	   (display "define?\n")
	   (eq? (car exp) 'define))

	 (define (eval-define exp env)
	   (display "eval-define\n")
	   (extend-env *env* (cadr exp) (eval-exp (caddr exp) env)))

	 (define (is-set!? exp)
	   (display "set!?\n")
	   (eq? (car exp) 'set!))

	 (define (eval-set! exp env)
	   (display "eval-set!\n")
	   (eval-define exp env))

	 (define (is-lambda? exp)
	   (display "lambda?\n")
	   (eq? (car exp) 'lambda))

	 (define (eval-lambda exp env)
	   (display "eval-lambda?\n")
	   (let ((params (cadr exp))
		 (body (caddr exp)))
	     (make-closure params body env)))

	 (define (is-plus? exp)
	   (display "is-plus?\n")
	   (eq? (car exp) '+))

	 (define (eval-plus exp env)
	   (display "eval-plus?\n")
	   (+ (eval-exp (cadr exp) env) 
	      (eval-exp (caddr exp) env)))

	 (define (is-minus? exp)
	   (display "is-minus?\n")
	   (eq? (car exp) '-))

	 (define (eval-minus exp env)
	   (display "eval-minus?\n")
	   (- (eval-exp (cadr exp) env) 
	      (eval-exp (caddr exp) env)))

	 (define (function-call-list? exp)
	   (display "function-call-list?\n")
	   (closure? (eval-symbol (car exp) *env*)))

	 (define (eval-function-call-list exp env)
	   (display "eval-function-call-list\n")
	   (let* ((function-name (car exp))
		  (function-args (map 
				  (lambda(arg) 
				    (eval-exp arg env))
				  (cdr exp)))

		  (closure (eval-symbol function-name env))
		  (function-body (closure-body closure))
		  (function-env (closure-env closure))
		  (function-params (closure-params closure)))

	     (let ((frame (create-frame)))
	       (let extend
		   ((params function-params)
		    (args function-args))
		 (if (null? params)
		     '()
		     (begin (extend-frame frame (car params) (car args))
			    (extend (cdr params) (cdr args)))))
	       (set! function-env (prepend-frame-to-env function-env frame)))

	     (eval-exp function-body function-env)))
)
