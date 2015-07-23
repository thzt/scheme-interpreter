(library (utility scope)
	 (export extend-env
		 add-to-frame

		 make-closure
		 closure-params
		 closure-body
		 closure-env)
	 (import (rnrs))

	 (define (extend-env frame env)
	   (cons frame env))

	 (define (add-to-frame frame key value)
	   (cons (cons key value) frame))


	 ;closure
	 (define-record-type closure (fields params body env))
)
