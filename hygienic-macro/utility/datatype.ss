(library (utility datatype)
	 (export make-closure
		 closure?
		 closure-param
		 closure-body
		 closure-env

		 make-macro
		 macro?
		 macro-param
		 macro-body
		 macro-env

		 make-macro-binding
		 macro-binding?
		 macro-binding-value
		 macro-binding-eval-env)
	 (import (rnrs))

	 (define-record-type closure 
	   (fields param body env))

	 (define-record-type macro 
	   (fields param body env))

	 (define-record-type macro-binding
	   (fields value eval-env))
)
