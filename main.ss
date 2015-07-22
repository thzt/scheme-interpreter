(define (eval-exp exp env)
    (cond ((symbol? exp) (eval-symbol exp env))
          ((self-eval-exp? exp) (eval-self-eval-exp exp))
          ((special-form? exp) (eval-special-form exp env))
          (else (error 'eval-exp "invalid exp"))))

(define (eval-symbol exp env)
	(cdr (lookup exp env)))

(define (self-eval-exp? exp)
	(or (number? exp) (boolean? exp)))

(define (eval-self-eval-exp exp)
	exp)

(define (special-form? exp)
    (member (car exp) '(if define set!)))

(define (eval-special-form exp env)
	(let ((head (car exp)))
	     (cond
		((eq? head 'if) 1)
		((eq? head 'define) 2)
		((eq? head 'set!) 3)
		(else (error 'eval-special "invalid special form")))))

(define (lookup sym env)
    (if (null? env)
        (error 'lookup "failed to find symbol")
        (let ((item (assoc sym (car env))))
          (if item
              (cdr item)
              (lookup sym (cdr env))))))
