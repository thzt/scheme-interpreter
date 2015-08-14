(library (utility tool)
	 (export handle-decision-tree
		 create-frame
		 extend-frame
		 extend-env
		 get-symbol-value)
	 (import (rnrs))

	 (define (handle-decision-tree tree exp env cont)
	   (if (null? tree)
	       (error 'handle-decision-tree "failed to make decision")
	       (let* ((head (car tree))
		      (predicator (car head))
		      (decision (cadr head)))

		 (predicator exp env 
			     (lambda (predicate-result)
			       (if predicate-result
				   (if (not (list? decision))
				       (decision exp env cont)
				       (handle-decision-tree decision exp env cont))
				   (handle-decision-tree (cdr tree) exp env cont)))))))

	 (define (create-frame)
	   (make-eq-hashtable))

	 (define (extend-frame frame key value)
	   (hashtable-set! frame key value))

	 (define (extend-env env frame)
	   (cons frame env))

	 (define (get-symbol-value env key)
	   (let lookup-env
	       ((env env))
	     (if (null? env)
		 (error 'get-symbol-value "failed to find symbol")
		 (let ((head-frame (car env)))
		   (if (hashtable-contains? head-frame key)
		       (hashtable-ref head-frame key '())
		       (lookup-env (cdr env)))))))
)
