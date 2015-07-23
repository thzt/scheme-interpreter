(import (rnrs) (utility tool) (utility scope))

					;export
(define (eval-exp exp env)
  (handle-case EVAL-CASES exp env))

					;eval case
(define (self-eval-exp? exp)
  (or (number? exp) (boolean? exp)))

(define (special-form? exp)
  (member (car exp) '(if define set! lambda)))

(define (eval-symbol sym env)
  (let lookup
      ((sym sym)
       (env env))
    (if (null? env)
	(error 'lookup "failed to find symbol")
	(let ((item (assoc sym (car env))))
	  (if item
	      (cdr item)
	      (lookup sym (cdr env)))))))

(define (eval-self-eval-exp exp env)
  exp)

(define (eval-special-form exp env)
  (handle-case EVAL-SPECIAL-CASES exp env))

(define EVAL-CASES
  `((,symbol? . ,eval-symbol)
    (,self-eval-exp? . ,eval-self-eval-exp)
    (,special-form? . ,eval-special-form)))

					;eval special case
(define (if? exp)
  (eq? (car exp) 'if))

(define (define? exp)
  (eq? (car exp) 'define))

(define (set!? exp)
  (eq? (car exp) 'set!))

(define (lambda? exp)
  (eq? (car exp) 'lambda))

(define (eval-if exp env)
  (if (eval-exp (cadr exp) env)
      (eval-exp (caddr exp) env)
      (eval-exp (cadddr exp) env)))

(define (eval-define exp env)
  (set! *top-level-frame* 
	(add-to-frame *top-level-frame*
		      (cadr exp) (eval-exp (caddr exp) env))))

(define (eval-set! exp env)
  3)

(define (eval-lambda exp env)
  (make-closure exp env))

(define EVAL-SPECIAL-CASES
  `((,if? . ,eval-if)
    (,define? . ,eval-define)
    (,set!? . ,eval-set!)
    (,lambda? . ,eval-lambda)))

					;top level
(define *top-level-frame* '())

					;test
(eval-exp '(if #t 1 2) '())
(eval-exp '(define a 1) '())
(eval-exp '(set!) '())
(eval-exp '(lambda) '())

(display *top-level-frame*)
