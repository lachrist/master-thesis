#lang racket

(require "type.rkt" "dico.rkt" "set.rkt")

(provide locus:intersect
         locus:safe-intersect
         locus:universe
         locus:from-constraint
         locus:from-constraints
         locus:from-dico
         locus:print
         locus:project
         locus:split
         locus:substitute
         locus:closure)

(struct locus (dico) #:transparent)

(define (locus:print l)
  (dico:print (locus-dico l) type:print))

(define (locus:universe)
  (locus (dico:make)))

(define (locus:from-constraint t1 t2)
  (locus (type:unify (list t1) (list t2))))

(define (locus:from-constraints ts1 ts2)
  (locus (type:unify ts1 ts2)))

(define (locus:from-dico dico)
  (locus (type:unify (dico:keys dico) (dico:values dico))))

(define (locus:project l type)
  (type:map-var type (lambda (var) (dico:lookup-if-any (locus-dico l) var))))

(define (locus:intersect l1 l2)
  (if (or (null? l1) (null? l2))
      null
      (locus (type:unify (append (dico:keys (locus-dico l1)) (dico:keys (locus-dico l2)))
                         (append (dico:values (locus-dico l1)) (dico:values (locus-dico l2)))))))

(define (locus:safe-intersect l1 l2)
  (call/cc (lambda (cont)
             (call-with-exception-handler
              (lambda (exn) (if (string? exn)
                                (cont '())
                                exn))
              (lambda () (locus:intersect l1 l2))))))

(define (locus:closure l links)
  (define (fix closure)
    (define next-closure closure)
    (dico:for-each-bind (locus-dico l) (lambda (key value)
                                         (define vars (set:add (type:vars value) key))
                                         (when (not (set:subset? vars next-closure))
                                           (set! next-closure (set:remove-set next-closure vars)))))
    (if (set:equal? closure next-closure)
        closure
        (fix next-closure)))
  (fix links))

(define (locus:split l closure)
  (let ((pair (dico:split-bind (locus-dico l) (lambda (key value) (not (set:empty? (set:intersect closure (set:add (type:vars value) key))))))))
    (cons (locus (car pair)) (locus (cdr pair)))))

(define (locus:substitute l dico)
  (if (null? l)
      null
      (locus:from-constraints (map (lambda (var) (dico:lookup-if-any dico var)) (dico:keys (locus-dico l)))
                              (map (lambda (type) (type:substitute type dico)) (dico:values (locus-dico l))))))