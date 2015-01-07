; Some utility functions that you may find useful.
(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

; Problem 18

;; Merge two lists LIST1 and LIST2 according to COMP and return
;; the merged lists.
(define (merge comp list1 list2)
    ; *** YOUR CODE HERE ***
    (cond ((null? list1) list2)
          ((null? list2) list1)
          (else (if (comp (car list1) (car list2))
                (cons (car list1) (merge comp (cdr list1) list2))
                (cons (car list2) (merge comp list1 (cdr list2)))))))

(merge < '(1 5 7 9) '(4 8 10))
; expect (1 4 5 7 8 9 10)
(merge > '(9 7 5 1) '(10 8 4 3))
; expect (10 9 8 7 5 4 3 1)

;; Sort a list of lists of numbers to be in decreasing lexicographic
;; order. Relies on a correct implementation of merge.
(define (sort-lists lsts)
  (if (or (null? lsts) (null? (cdr lsts)))
      lsts
      (let ((sublsts (split lsts)))
        (merge greater-list
               (sort-lists (car sublsts))
               (sort-lists (cdr sublsts))))))

(define (greater-list x y)
  (cond ((null? y) #t)
        ((null? x) #f)
        ((> (car x) (car y)) #t)
        ((> (car y) (car x)) #f)
        (else (greater-list (cdr x) (cdr y)))))

(define (split x)
  (cond ((or (null? x) (null? (cdr x))) (cons x nil))
        (else (let ((sublsts (split (cdr (cdr x)))))
                (cons (cons (car x) (car sublsts))
                      (cons (car (cdr x)) (cdr sublsts)))))))

(merge greater-list '((3 2 1) (1 1) (0)) '((4 0) (3 2 0) (3 2) (1)))
; expect ((4 0) (3 2 1) (3 2 0) (3 2) (1 1) (1) (0))


; Problem 19

;; A list of all ways to partition TOTAL, where  each partition must
;; be at most MAX-VALUE and there are at most MAX-PIECES partitions.
;(define (list-partitions total max-pieces max-value)
 ; ; *** YOUR CODE HERE ***
  ;(construct total max-value)
 ; (cond ((= total 0) 1)
  ;      ((< total 0) 0)
   ;     ((= max-value 0) 0)
    ;    (else (+ (list-partitions (- total max-value) max-pieces max-value) (list-partitions total max-pieces (- max-value 1))))))

;(define (construct total max-value)
 ;   (cons (total max-value)))
(define (length seq)
  (if (null? seq)
    0
    (+ 1 (length (cdr seq)))))

(define (list-partitions total max-pieces max-value)
  (filter (lambda (x) (< (length x) (+ 1 max-pieces))) (construct-partitions total max-pieces max-value nil)))


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


; Problem 19 tests rely on correct Problem 18.
;(sort-lists (list-partitions 5 2 4))
; expect ((4 1) (3 2))
;(sort-lists (list-partitions 7 3 5))
; expect ((5 2) (5 1 1) (4 3) (4 2 1) (3 3 1) (3 2 2))


; Problem 20

;; The Tree abstract data type has an entry and a list of children.
(define (make-tree entry children)
  (cons entry children))
(define (entry tree)
  (car tree))
(define (children tree)
  (if (null? tree) okay
  (cdr tree)))

;; An example tree:
;;                5
;;       +--------+--------+
;;       |        |        |
;;       6        7        2
;;    +--+--+     |     +--+--+
;;    |     |     |     |     |
;;    9     8     1     6     4
;;                      |
;;                      |
;;                      3
(define tree
  (make-tree 5 (list
                (make-tree 6 (list
                              (make-tree 9 nil)
                              (make-tree 8 nil)))
                (make-tree 7 (list
                              (make-tree 1 nil)))
                (make-tree 2 (list
                              (make-tree 6 (list
                                            (make-tree 3 nil)))
                              (make-tree 4 nil))))))

;; Takes a TREE of numbers and outputs a list of sums from following each
;; possible path from root to leaf.
'(5 (6 (9) (8)) (7 (1)) (2 (6 (3)) (4)))

(define (cadr tree)
  (car (cdr tree)))

(define (cdar tree)
  (cdr (car tree)))

(define (caar tree)
  (car (car tree)))


(define (tree-sums tree)
    ; *** YOUR CODE HERE ***
    (tree-sums-helper (cons tree nil) nil 0))

(define (tree-sums-helper sub-tree leaf sum)
   (if (null? sub-tree)
       leaf
       (if (null? (cdar sub-tree))
          (append leaf (cons (+ (caar sub-tree) sum) nil)
                (tree-sums-helper (children sub-tree) leaf sum))
          (append (tree-sums-helper (cdar sub-tree) leaf (+ (caar sub-tree) sum))
                (tree-sums-helper (children sub-tree) leaf sum)
          ))
      ))

(define (flatten lst)
  (if (null? lst)
      nil
      (cons (car lst) (flatten (cdr lst)))))

(define (flatten2 lst)
    (if (null? lst)
        nil
        (cons (find-first lst) (flatten2 (cdr lst)))))

(define (add-num-to-list num lst)
    (if (null? lst)
        nil
        (cons (+ num (find-first lst)) (add-num-to-list num (cdr lst)))))

;(define (tree-sums tree)
  ; *** YOUR CODE HERE ***
 ;   (define (helper a_tree child_sums total)
  ;    (if (null? a_tree)
   ;     child_sums
    ;    (begin (define tree (entry a_tree))
     ;     (if (null? (children tree))
      ;       (append (append child_sums (cons (+ (entry tree) total) nil) 
       ;       (helper (children a_tree) child_sums total)))
        ;  (append (helper (children tree) child_sums (+ (entry tree) total)) 
         ;       (helper (children asub-tree) child_sums total))))))
        ;(helper (cons tree nil) '() 0))




;(define (flatten lst)
 ; (if (null? lst)
  ;    nil
   ;   (list (car lst)
    ;          (flatten (cdr lst)))))

;(define (leaf? s) (not (pair? s)))
;(define lst '())

;(define (list-sum a lst)
 ; (if (null? lst) nil
  ;  (cons (+ a (car lst)) (list-sum a (cdr lst)))))

;(define (tree-sum-list tree)
 ; (cond
  ;  ((number? tree) (cons tree nil))
   ; ((null? (children tree)) (cons (entry tree) nil))
    ;(else (tree-sum-list (entry tree)) 
     ; (add-num-to-list (find-first tree) (flatten (children (flatten (tree-sum-list 
      ;  (children tree)))))))))


;(define (tree-sums tree)
;
 ; (+ (entry tree) (flatten
  ;(define n (length (children tree)))
  ;(define (for n)
   ; (if (= n 0)
    ;  nil
     ; (flatten (tree-sums (getitem (children tree) n)))
      ;(for (- n 1))
      ;)))
  ;))


;(define (tree-sums tree)
 ;   (if (or (number? tree) (null? tree))
  ;      nil
   ;     (if (or (number? tree) (null? (cdr tree)))
    ;        (find-first tree)
     ;       (cons (tree-sums (car (cdr tree)))
      ;            (cons (tree-sums (cdr (cdr tree))))))))
        ;(add-num-to-list (flatten2 (cdr tree)) (car tree))))

;; An example tree:
;;                5
;;       +--------+--------+
;;       |        |        |
;;       6        7        2
;;    +--+--+     |     +--+--+
;;    |     |     |     |     |
;;    9     8     1     6     4
;;                      |
;;                      |
;;                      3

;(define (tree-sums tree)
 ;       (define (helper1 tree)
  ;      (if (= (length tree) 1)
   ;         (find-first tree)
    ;        (+ (find-first tree) (helper1 (car (cdr tree))))))

     ;   (define (helper2 tree)
      ;      (cond ((= (length tree) 1) (find-first tree)) ;(or (null? tree) (null? (cdr tree)))
       ;             ((null? (or (cdr tree))) (find-first tree))
        ;            ((null? (cdr (cdr tree))) (find-first tree))
         ;           (else (+ (find-first tree) (helper2 (car (cdr (cdr tree))))))))
        
     ;   (cons (helper1 tree)
      ;        (cons (helper2 tree) nil)
       ; ))






(define (connect lst1 lst2)
    (if (null? lst1) 
        lst2
        (cons (car lst1) (connect (cdr lst1) lst2))))

(define (getitem lst n)
    (if (= n 1)
        (car lst)
        (getitem (cdr lst) (- n 1))))

(define (add-lst lst1 lst2)
    (if (null? lst1)
        nil
        (cons (+ (car lst1) (car lst2))
            (add-lst (cdr lst1) (cdr lst2)))))

(define (find-first tree)
    (cond ((number? tree) tree)
          ((null? tree) 0)
          (else (find-first (car tree)))))

(define (length tree)
    (if (null? tree)
        0
        (+ 1 (length (cdr tree)))))



;(tree-sum-list tree)
(tree-sums tree)
; expect (20 19 13 16 11)


; Problem 21 (optional)

; Draw the hax image using turtle graphics.
(define (hax d k)
  ; *** YOUR CODE HERE ***
  nil)
