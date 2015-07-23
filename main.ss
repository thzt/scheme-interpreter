(import (rnrs) 
	(utility tool) 
	(utility scope))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;entrance

(define (eval exp env)
  (handle-case (create-eval-case) exp env))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;eval case

(define (create-eval-case)
  `((,symbol? . ,eval-symbol)
    (,self-eval-exp? . ,eval-self-eval-exp)
    (,list? . ,eval-list)))

;private

(define (self-eval-exp? exp)
  (or (number? exp) (boolean? exp)))

(define (eval-symbol sym env)
  (display "eval-symbol\n")
  (get-symbol-value-from-env env sym))

(define (eval-self-eval-exp exp env)
  exp)

(define (eval-list exp env)
  (display "eval-list\n")
  (handle-case (create-eval-list-case) exp env))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;eval list case 

(define (create-eval-list-case)
   `((,special-form-list? . ,eval-special-form-list)
    (,function-call-list? . ,eval-function-call-list)))

;private

(define (special-form-list? exp)
  (display "special-form-list?\n")
  (member (car exp) '(if define set! lambda)))

(define (function-call-list? exp)
  (closure? (eval-symbol (car exp) *env*)))

(define (eval-special-form-list exp env)
  (display "eval-special-form-list\n")
  (handle-case (create-eval-special-case) exp env))

(define (eval-function-call-list exp env)
  (display "eval-function-call-list\n")
  (let* ((function-name (car exp))
	 (function-args (cdr exp))

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

    (eval function-body function-env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;eval special case

(define (create-eval-special-case)
  `((,if? . ,eval-if)
    (,define? . ,eval-define)
    (,set!? . ,eval-set!)
    (,lambda? . ,eval-lambda)))

;private

(define (if? exp)
  (eq? (car exp) 'if))

(define (define? exp)
  (display "define?\n")
  (eq? (car exp) 'define))

(define (set!? exp)
  (eq? (car exp) 'set!))

(define (lambda? exp)
  (display "lambda?\n")
  (eq? (car exp) 'lambda))

(define (eval-if exp env)
  (if (eval (cadr exp) env)
      (eval (caddr exp) env)
      (eval (cadddr exp) env)))

(define (eval-define exp env)
  (display "eval-define\n")
  (extend-env *env* (cadr exp) (eval (caddr exp) env)))

(define eval-set!
  eval-define)

(define (eval-lambda exp env)
  (display "eval-lambda?\n")
  (let ((params (cadr exp))
	(body (caddr exp)))
    (make-closure params body env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;top level var

(define *env* `(,(create-frame)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;test

;(eval '(if #t 1 2))
(eval '(define id (lambda (x) x)) *env*)
;(display (eval 'id *env*))
(display (eval '(id 25) *env*))

;(display (get-symbol-value-from-env *env* 'id))
