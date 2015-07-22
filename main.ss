;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
					;main

(define (eval-exp exp env)
  (handle-case HANDLE-EVAL-CASE-LIST exp env))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
					;eval case

(define HANDLE-EVAL-CASE-LIST
  `((,symbol? . ,eval-symbol)
    (,self-eval-exp? . ,eval-self-eval-exp)
    (,special-form? . ,eval-special-form)))

(define (self-eval-exp? exp)
  (or (number? exp) (boolean? exp)))
(define (special-form? exp)
  (member (car exp) '(if define set!)))

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
  (handle-case  HANDLE-EVAL-SPECIAL-CASE-LIST exp env))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
					;eval special form case

(define HANDLE-EVAL-SPECIAL-CASE-LIST
  `((,if? . ,eval-if)
    (,define? . ,eval-define)
    (,set!? . ,eval-set!)
    (,lambda? . ,eval-lambda)))

(define (if? exp)
  (eq? (car exp) 'if))
(define (define? exp)
  (eq? (car exp) 'define))
(define (set!? exp)
  (eq? (car exp) 'set!))
(define (lambda? exp)
  (eq? (car exp) 'lambda))

(define (eval-if exp env)
  1)
(define (eval-define exp env)
  2)
(define (eval-set! exp env)
  3)
(define (eval-lambda exp env)
  4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
					;tools

(define (handle-case case-list exp env)
  (if (null? case-list)
      (error 'handle-case "all predicator return false.")
      (let* ((head (car case-list))
	     (predicate (car head))
	     (handler (cdr head)))
	(if (predicate exp)
	    (handler exp env)
	    (handle-case (cdr case-list) exp env)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
					;scope related

(define *top-level-env* '())

(define (extend-env frame env)
  (cons frame env))

(define (add-to-frame frame key value)
  (cons (cons key value) frame))
