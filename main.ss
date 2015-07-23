(import (rnrs) (utility eval) (utility scope))

;(eval '(if #t 1 2))
;(display (eval 'id *env*))

(eval-exp '(define add (lambda (x y) (+ x y))) *env*)
(eval-exp '(define minus (lambda (x y) (- x y))) *env*)
(display (eval-exp '(minus 3 (add 1 2)) *env*))

;(display (get-symbol-value-from-env *env* '+))
