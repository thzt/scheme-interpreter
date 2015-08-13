(library (utility tool)
	 (export handle-tree callback-reduce)
	 (import (rnrs))

	 (define (handle-tree tree exp env cont)
	   (if (null? tree)
	       (error 'handle-tree "error")
	       (let* ((head (car tree))
		      (predicator (car head))
		      (decision (cadr head)))

		 (predicator exp env
			     (lambda (v)
			       (if v
				   (if (not (list? decision))
				       (decision exp env cont)
				       (handle-tree decision exp env cont))
				   (handle-tree (cdr tree) exp env cont)))))))

	 (define (callback-reduce args result iterate complete)
	   (if (null? args)
	       (complete result)
	       (let ((head (car args)))
		 (iterate head
			  (lambda (v)
			    (let ((new-result (cons v result)))
			      (callback-reduce (cdr args) new-result iterate complete)))))))
)

