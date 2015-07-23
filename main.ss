(import (rnrs) (utility eval) (utility scope))

;(eval '(if #t 1 2))
(eval-exp '(define id (lambda (x) x)) *env*)
;(display (eval 'id *env*))
(display (eval-exp '(id 25) *env*))

;(display (get-symbol-value-from-env *env* 'id))
