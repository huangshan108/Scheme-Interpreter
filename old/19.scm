(define (length seq)
	(if (null? seq)
		0
		(+ 1 (length (cdr seq)))))

(define (list-partitions total max-pieces max-value)
	(filter (lambda (x) (< (length x) (+ 1 max-pieces))) (construct-partitions total max-pieces max-value nil)))

(define (merge comp list1 list2)
  (cond ((null? list1) list2)
        ((null? list2) list1)
        (else (if (comp (car list1) (car list2))
              (cons (car list1) (merge comp (cdr list1) list2))
              (cons (car list2) (merge comp list1 (cdr list2)))))))

(define (construct-partitions total max-pieces max-value partitions)
  (cond ((eq? total 0) (cons partitions nil))
  		((< total 0) nil)
		((eq? max-value 0) nil)
		;((eq? (length partitions) (- max-pieces 1)) (cons (merge > (cons total nil) partitions) nil))
		(else ;(if (null? partitions)
				;(cons (construct-partitions (- total max-value) max-pieces max-value (cons max-value nil))
					;(construct-partitions total max-pieces (- max-value 1) partitions))
				(append (construct-partitions (- total max-value) max-pieces max-value (append partitions (cons max-value nil)))
					(construct-partitions total max-pieces (- max-value 1) partitions))))
					)

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))
