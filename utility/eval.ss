(library (utility eval)
	 (export eval-exp *env* *cont*)
	 (import (rnrs) (utility tool) (utility scope))
	 
	 ;export

	 (define *env* `(,(create-frame)))
	 
	 (define *cont* (lambda (x) (display x)))

	 (define (eval-exp exp env cont)
	   (handle-tree 
	    `((,is-symbol? ,eval-symbol)
	      (,self-eval-exp? ,eval-self-eval-exp)
	      (,is-continuation? ,eval-continuation)
	      (,is-list?
	       ((,special-form-list?
		 ((,is-if? ,eval-if)
		  (,is-define? ,eval-define)
		  (,is-set!? ,eval-set!)
		  (,is-lambda? ,eval-lambda)
		  (,is-plus? ,eval-plus)
		  (,is-minus? ,eval-minus)
		  (,is-call/cc? ,eval-call/cc)))
		(,is-continuation-call? ,eval-continuation-call)
		(,function-call-list? ,eval-function-call-list))))
	    exp env cont))

	 ;private region

	 (define (is-symbol? exp env cont)
	   (display "is-symbol?\n")
	   (cont (symbol? exp)))

	 (define (eval-symbol exp env cont)
	   (display "eval-symbol\n")
	   (cont (get-symbol-value-from-env env exp)))

	 (define (self-eval-exp? exp env cont)
	   (display "self-eval-exp?\n")
	   (cont (or (number? exp) (boolean? exp))))

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

	 (define (special-form-list? exp env cont)
	   (display "special-form-list?\n")
	   (cont (member (car exp) '(if define set! lambda + - call/cc))))

	 (define (is-if? exp env cont)
	   (display "if?\n")
	   (cont (eq? (car exp) 'if)))

	 (define (eval-if exp env cont)
	   (display "eval-if\n")
	   (eval-exp (cadr exp) env 
		     (lambda(v)
		       (if v
			   (eval-exp (caddr exp) env cont)
			   (eval-exp (cadddr exp) env cont)))))

	 (define (is-define? exp env cont)
	   (display "define?\n")
	   (cont (eq? (car exp) 'define)))

	 (define (eval-define exp env cont)
	   (display "eval-define\n")
	   (eval-exp (caddr exp) env
		     (lambda (v)
		       (cont (extend-env *env* (cadr exp) v)))))

	 (define (is-set!? exp env cont)
	   (display "set!?\n")
	   (cont (eq? (car exp) 'set!)))

	 (define (eval-set! exp env cont)
	   (display "eval-set!\n")
	   (eval-define exp env cont))

	 (define (is-lambda? exp env cont)
	   (display "lambda?\n")
	   (cont (eq? (car exp) 'lambda)))

	 (define (eval-lambda exp env cont)
	   (display "eval-lambda?\n")
	   (let ((params (cadr exp))
		 (body (caddr exp)))
	     (cont (make-closure params body env))))

	 (define (is-plus? exp env cont)
	   (display "is-plus?\n")
	   (cont (eq? (car exp) '+)))

	 (define (eval-plus exp env cont)
	   (display "eval-plus?\n")
	   (eval-exp (cadr exp) env
		     (lambda (v)
		       (eval-exp (caddr exp) env
				 (lambda (w)
				   (cont (+ v w)))))))

	 (define (is-minus? exp env cont)
	   (display "is-minus?\n")
	   (cont (eq? (car exp) '-)))

	 (define (eval-minus exp env cont)
	   (display "eval-minus?\n")
	   (eval-exp (cadr exp) env
		     (lambda (v)
		       (eval-exp (caddr exp) env
				 (lambda (w)
				   (cont (- v w)))))))
	 
	 (define (is-call/cc? exp env cont)
	   (display "is-call/cc?\n")
	   (cont (eq? (car exp) 'call/cc)))

	 (define (eval-call/cc exp env cont)
	   (display "eval-call/cc\n")
	   (let ((wrapped-cont (make-continuation cont)))
	     (eval-lambda (cadr exp) env
			  (lambda (v)
			    (eval-function-call-list `(,v ,wrapped-cont) env cont)))))

	 (define (is-continuation-call? exp env cont)
	   (display "is-continuation-call?\n")
	   (cont (continuation? 
		  (get-symbol-value-from-env env (car exp)))))
	  
	 (define (eval-continuation-call exp env cont)
	   (display "eval-continuation-call\n")
	   (eval-symbol (car exp) env
			(lambda (v)
			  (let ((original-cont (continuation-cont v)))
			    (eval-exp (cadr exp) env
				      (lambda (v)
					(original-cont v)))))))

	 (define (function-call-list? exp env cont)
	   (display "function-call-list?\n")
	   (eval-symbol (car exp) *env*
			(lambda (v)
			  (cont (closure? v)))))

	 (define (eval-closure closure function-args cont)
	   (let ((function-body (closure-body closure))
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
	     
	     (eval-exp function-body function-env
		       (lambda (v)
			 (cont v)))))

	 (define (eval-function-call-list exp env cont)
	   (display "eval-function-call-list\n")

	   (let ((function-name (car exp)))
	     (callback-reduce (cdr exp)
			 '()
			 (lambda (arg next)
			   (eval-exp arg env
				     (lambda (v)
				       (next v))))
			 (lambda (function-args)
			   (if (closure? function-name)
			       (eval-closure function-name function-args cont)
			       (eval-symbol function-name env
					    (lambda (closure)
					      (eval-closure closure function-args cont))))))))
)
