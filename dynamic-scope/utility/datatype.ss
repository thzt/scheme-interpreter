(library (utility datatype)
	 (export make-function
		 function?
		 function-param
		 function-body)
	 (import (rnrs))

	 (define-record-type function 
	   (fields param body))
)
