(library (utility datatype)
	 (export make-closure
		 closure?
		 closure-param
		 closure-body
		 closure-env)
	 (import (rnrs))

	 (define-record-type closure 
	   (fields param body env))
)
