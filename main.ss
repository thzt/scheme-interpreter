(import (rnrs) 
	(utility tool) 
	(utility scope))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;entrance

(define (eval-exp exp)
  (handle-case (create-eval-case) exp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;eval case

(define (create-eval-case)
  `((,symbol? . ,eval-symbol)
    (,self-eval-exp? . ,eval-self-eval-exp)
    (,list? . ,eval-list)))

;private

(define (self-eval-exp? exp)
  (or (number? exp) (boolean? exp)))

(define (eval-symbol sym)
  (get-symbol-value-from-env *env*))

(define (eval-self-eval-exp exp)
  exp)

(define (eval-list exp)
  (handle-case (create-eval-list-case) exp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;eval list case 

(define (create-eval-list-case)
   `((,special-form-list? . ,eval-special-form-list)
    (,function-call-list? . ,eval-function-call-list)))

;private

(define (special-form-list? exp)
  (member (car exp) '(if define set! lambda)))

(define (function-call-list? exp)
  (closure? (eval-symbol (car exp))))

(define (eval-special-form-list exp)
  (handle-case (create-eval-special-case) exp))

(define (eval-function-call-list exp)
  (display 123))

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
  (eq? (car exp) 'define))

(define (set!? exp)
  (eq? (car exp) 'set!))

(define (lambda? exp)
  (eq? (car exp) 'lambda))

(define (eval-if exp)
  (if (eval-exp (cadr exp))
      (eval-exp (caddr exp))
      (eval-exp (cadddr exp))))

(define (eval-define exp)
  (extend-env *env* (cadr exp) (eval-exp (caddr exp))))

(define (eval-set! exp)
  (display "eval-set!"))

(define (eval-lambda exp)
  (let ((params (cadr exp))
	(body (caddr exp)))
    (make-closure params body *env*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;top level var

(define *env* `(,(create-frame)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;test

;(eval-exp '(if #t 1 2))
(eval-exp '(define id (lambda (x) x)))
;(eval-exp '(set!))
;(eval-exp '(id 2))

(display (get-symbol-value-from-env *env* 'id))
