#lang racket

(require "misc.rkt")

(provide set:make-empty
         set:to-list
         set:from-list
         set:intersect
         set:for-each
         set:print
         set:add
         set:add-list
         set:add-set
         set:remove
         set:remove-list
         set:remove-set
         set:contain?
         set:empty?
         set:subset?
         set:equal?)

(struct set (elems) #:transparent)

;;;;;;;;;;;;;
;; General ;;
;;;;;;;;;;;;;

(define (set:make-empty)
  (set '()))

(define (set:to-list s)
  (set-elems s))

(define (set:from-list list)
  (set:add-list (set:make-empty) list))

(define (set:intersect s1 s2)
  (define elems '())
  (for-each (lambda (elem1) (when (contain? (set-elems s2) elem1) (set! elems (cons elem1 elems)))) (set-elems s1))
  (set elems))

(define (set:for-each s proc!)
  (for-each proc! (set-elems s)))

(define (set:print s print-element)
  (string-append "{" (print-list (set-elems s) print-element " ") "}"))

;;;;;;;;;
;; Add ;;
;;;;;;;;;

(define (set:add s elem)
  (if (contain? (set-elems s) elem)
      s
      (set (cons elem (set-elems s)))))

(define (set:add-list s xs)
  (for-each (lambda (x) (set! s (set:add s x))) xs)
  s)

(define (set:add-set s1 s2)
  (set:add-list s1 (set-elems s2)))

;;;;;;;;;;;;
;; Remove ;;
;;;;;;;;;;;;

(define (set:remove s elem)
  (define (loop elems)
    (cond ((null? elems) '())
          ((eq? elem (car elems)) (loop (cdr elems)))
          (else (cons (car elems) (loop (cdr elems))))))
  (set (loop (set-elems s))))

(define (set:remove-list s l)
  (for-each (lambda (elem) (set! s (set:remove s elem))) l)
  s)

(define (set:remove-set s1 s2)
  (set:remove-list s1 (set-elems s2)))

;;;;;;;;;;;;;;;
;; Predicate ;;
;;;;;;;;;;;;;;;

(define (set:contain? s e)
  (contain? (set-elems s) e))

(define (set:subset? s1 s2)
  (define subset? #t)
  (for-each (lambda (elem1) (when (not (contain? (set-elems s2) elem1)) (set! subset? #f))) (set-elems s1))
  subset?)

(define (set:equal? s1 s2)
  (and (set:subset? s1 s2) (set:subset? s2 s1)))

(define (set:empty? s)
  (null? (set-elems s)))