#lang racket
(require "misc.rkt")

(provide dico:make
         dico:keys
         dico:values
         dico:dico?
         dico:add
         dico:add!
         dico:add-all
         dico:add-all!
         dico:lookup
         dico:lookup-if-any
         dico:contain?
         dico:map-value
         dico:map-key
         dico:for-each-bind
         dico:map-bind
         dico:select
         dico:remove
         dico:split-bind
         dico:pick
         dico:empty?
         dico:print
         dico:remove-all)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dico :: [(object,object)] ;;
;; use eq? to lookup         ;;
;; only one value epr keys   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct dico (binds) #:transparent #:mutable)

(define dico:dico? dico?)
(define (dico:make)
  (dico '()))

(define (dico:map-value d proc)
  (dico (map (lambda (bind) (cons (car bind) (proc (cdr bind)))) (dico-binds d))))

(define (dico:map-key d proc)
  (dico (map (lambda (bind) (cons (proc (car bind)) (cdr bind))) (dico-binds d))))

(define (dico:lookup d key fail)
  (define (perform binds)
    (if (null? binds)
        (fail)
        (if (eq? key (car (car binds)))
            (cdr (car binds))
            (perform (cdr binds)))))
  (perform (dico-binds d)))

(define (dico:lookup-if-any d key)
  (dico:lookup d key (lambda () key)))

(define (dico:contain? d key)
  (any-satisfy? (dico-binds d) (lambda (bind) (eq? (car bind) key))))

(define (dico:remove d key)
  (define (perform binds)
    (cond ((null? binds) '())
          ((eq? (car (car binds)) key) (cdr binds))
          (else (cons (car binds) (perform (cdr binds))))))
  (dico (perform (dico-binds d))))

(define (dico:add! d key value)
  (set-dico-binds! d (dico-binds (dico:add d key value))))

(define (dico:add-all! d keys values)
  (set-dico-binds! d (dico-binds (dico:add-all d keys values))))

(define (dico:add d key value)
  (dico (cons (cons key value) (dico-binds (dico:remove d key)))))

(define (dico:remove-all d keys)
  (define (perform binds)
    (cond ((null? binds) '())
          ((contain? keys (car (car binds))) (perform (cdr binds)))
          (else (cons (car binds) (perform (cdr binds))))))
  (dico (perform (dico-binds d))))

(define (dico:add-all d keys values)
  (define (loop binds keys values)
    (if (or (null? keys) (null? values))
        (dico binds)
        (if (contain? (cdr keys) (car keys))
            (loop binds (cdr keys) (cdr values))
            (loop (cons (cons (car keys) (car values)) binds) (cdr keys) (cdr values)))))
  (loop (dico-binds (dico:remove-all d keys)) keys values))

(define (dico:select d keys)
  (define (perform binds)
    (if (null? binds)
        '()
        (if (contain? keys (car (car binds)))
            (cons (car binds) (perform (cdr binds)))
            (perform (cdr binds)))))
  (dico (perform (dico-binds d))))

(define (dico:empty? d)
  (null? (dico-binds d)))

(define (dico:keys d)
  (map car (dico-binds d)))

(define (dico:values d)
  (map cdr (dico-binds d)))

(define (dico:pick d)
  (cons (car (dico-binds d)) (dico (cdr (dico-binds d)))))

(define (dico:for-each-bind d proc!)
  (for-each (lambda (bind) (proc! (car bind) (cdr bind))) (dico-binds d)))

(define (dico:split-bind d pred?)
  (define (perform binds ins outs)
    (cond ((null? binds) (cons (dico ins) (dico outs)))
          ((pred? (car (car binds)) (cdr (car binds))) (perform (cdr binds) (cons (car binds) ins) outs))
          (else (perform (cdr binds) ins (cons (car binds) outs)))))
  (perform (dico-binds d) '() '()))

(define (dico:map-bind d proc)
  (dico (map (lambda (bind) (proc (car bind) (cdr (bind)))) (dico-binds d))))

(define (dico:print d print-element)
  (string-append "{" (print-list (dico-binds d) (lambda (bind) (string-append (print-element (car bind)) " => " (print-element (cdr bind)))) ", ") "}"))
        