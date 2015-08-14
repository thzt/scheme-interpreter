(library (utility datatype)
	 (export make-closure
		 closure?
		 closure-param
		 closure-body
		 closure-env

		 make-continuation
		 continuation?
		 continuation-cont)
	 (import (rnrs))

	 (define-record-type closure 
	   (fields param body env))

	 (define-record-type continuation
	   (fields cont))
)
