#lang racket

(provide tagged-list?
         all-satisfy?
         any-satisfy?
         all-different?
         zip
         contain?
         print-list
         split
         fold)

(define (print-list list print-element sep)
  (define (loop list)
    (if (null? list)
        ""
        (string-append sep (print-element (car list)) (loop (cdr list)))))
  (if (null? list)
      ""
      (string-append (print-element (car list)) (loop (cdr list)))))

(define (split list pred?)
  (define (perform xs ins outs)
    (cond ((null? xs) (cons ins outs))
          ((pred? (car xs)) (perform (cdr xs) (cons (car xs) ins) outs))
          (else (perform (cdr xs) ins (cons (car xs) outs)))))
  (perform list '() '()))

(define (tagged-list? object tag)
  (and (list? object) (not (null? object)) (eq? tag (car object))))

(define (all-satisfy? list pred?)
  (cond ((null? list) #t)
        ((pred? (car list)) (all-satisfy? (cdr list) pred?))
        (else #f)))

(define (all-different? list)
  (cond ((null? list) #t)
        ((contain? (cdr list) (car list)) #f)
        (else (all-different? (cdr list)))))

(define (any-satisfy? list pred?)
  (cond ((null? list) #f)
        ((pred? (car list)) #t)
        (else (any-satisfy? (cdr list) pred?))))

(define (fold list acc zero)
  (if (null? list)
      zero
      (acc (car list) (fold (cdr list) acc zero))))

(define (zip list1 list2)
  (if (or (null? list1) (null? list2))
      '()
      (cons (cons (car list1) (car list2)) (zip (cdr list1) (cdr list2)))))

(define (contain? list element)
  (any-satisfy? list (lambda (inner) (eq? inner element))))