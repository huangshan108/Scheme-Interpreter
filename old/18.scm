(define (merge comp list1 list2)
     ;*** YOUR CODE HERE ***
    (if (null? list2)
        list2
        (if (comp (car list1) (car list2))  
            (cons (car list1) (cons (car list2) (merge comp (cdr list1) (cdr list2))))
            (cons (car list2) (cons (car list1) (merge comp (cdr list1) (cdr list2))))
            ;))
         )))()