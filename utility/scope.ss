(library (utility scope)
	 (export create-frame
		 extend-frame
		 prepend-frame-to-env

		 extend-env
		 get-symbol-value-from-env

		 make-closure
		 closure?
		 closure-params
		 closure-body
		 closure-env)
	 (import (rnrs))

	 ;frame operation
	 (define (create-frame)
	   (make-eq-hashtable))

	 (define (extend-frame frame key value)
	   (hashtable-set! frame key value))

	 (define (prepend-frame-to-env env frame)
	   (cons frame env))

	 ;env operation
	 (define (extend-env env key value)
	   (let ((head-frame (car env)))
	     (hashtable-set! head-frame key value)))

	 (define (get-symbol-value-from-env env key)
	   (let lookup-env
	       ((env env))
	     (if (null? env)
		 (error 'get-symbol-value-from-env "failed to find symbol")
		 (let ((head-frame (car env)))
		   (if (hashtable-contains? head-frame key)
		       (hashtable-ref head-frame key '())
		       (lookup-env (cdr env)))))))

	 ;closure
	 (define-record-type closure (fields params body env))
)
