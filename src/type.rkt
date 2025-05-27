#lang racket

(require "dico.rkt" "misc.rkt" "set.rkt")

(provide type:map-var
         type:vars
         type:unify
         type:print
         type:substitute
         type:make-var!
         type:make-num
         type:make-abs
         type:make-pair
         type:make-list
         type:make-bool
         type:make-str
         type:make-void
         type:make-ptype
         type:ptype?
         type:ptype-subst
         type:ptype-inst)

(struct type () #:transparent)
(struct var type (counter) #:transparent)
(struct comp type (tag types) #:transparent)
(struct ptype type (links mtype) #:transparent)

;;;;;;;;;;;;;;;
;; Poly type ;;
;;;;;;;;;;;;;;;
(define type:make-ptype ptype)
(define type:ptype? ptype?)
(define (type:ptype-subst pt)
  (define dico (dico:make))
  (set:for-each (ptype-links pt) (lambda (var) (set! dico (dico:add dico var (type:make-var!)))))
  dico)
(define (type:ptype-inst pt dico)
  (type:substitute (ptype-mtype pt) dico))

;;;;;;;;;;;;;;
;; Variable ;;
;;;;;;;;;;;;;;
(define *counter* 0)
(define (type:make-var!)
  (set! *counter* (+ *counter* 1))
  (var *counter*))

;;;;;;;;;;;;;;;;;;;
;; Compound type ;;
;;;;;;;;;;;;;;;;;;;
;; abs
(define (type:make-abs args res)
  (comp 'abs (cons res args)))
(define (type:abs? comp)
  (eq? (comp-tag comp) 'abs))
(define (type:abs-args abs)
  (cdr (comp-types abs)))
(define (type:abs-res abs)
  (car (comp-types abs)))
;; pair
(define (type:make-pair fst scd)
  (comp 'pair (list fst scd)))
(define (type:pair? comp)
  (eq? (comp-tag comp) 'pair))
(define (type:pair-fst pair)
  (car (comp-types pair)))
(define (type:pair-scd pair)
  (car (cdr (comp-types pair))))
;; List
(define (type:make-list inner)
  (comp 'list (list inner)))
(define (type:list? comp)
  (eq? (comp-tag comp) 'list))
(define (type:list-inner list)
  (car (comp-types list)))
;; Num
(define (type:make-num)
  (comp 'num '()))
(define (type:num? comp)
  (eq? (comp-tag comp) 'num))
;; Bool
(define (type:make-bool)
  (comp 'bool '()))
(define (type:bool? comp)
  (eq? (comp-tag comp) 'bool))
;; Str
(define (type:make-str)
  (comp 'str '()))
(define (type:str? comp)
  (eq? (comp-tag comp) 'str))
;; Void
(define (type:make-void)
  (comp 'void '()))
(define (type:void? comp)
  (eq? (comp-tag comp) 'void))

;;;;;;;;;;;
;; Types ;;
;;;;;;;;;;;

(define (type:reach-var t reach proc)
  (define links (if (ptype? t)
                    (ptype-links t)
                    (set:make-empty)))
  (define (perform t)
    (cond ((set:contain? links t) t)
          ((var? t) (proc t))
          ((comp? t) (comp (comp-tag t) (reach perform (comp-types t))))
          (else (error "invalid type" t))))
  (if (ptype? t)
      (perform (ptype-mtype t))
      (perform t)))

(define (type:vars t)
  (define vars (set:make-empty))
  (type:for-each-var t (lambda (var) (set! vars (set:add vars var))))
  vars)

(define (type:map-var t proc)
  (type:reach-var t map proc))

(define (type:for-each-var t proc!)
  (type:reach-var t for-each proc!))

(define (type:contain-var? t var)
  (define in? #f)
  (type:for-each-var t (lambda (t) (when (eq? t var) (set! in? #t))))
  in?)

(define (type:substitute t dico)
  (type:map-var t (lambda (var) (dico:lookup-if-any dico var))))

(define (type:print t)
  (cond ((var? t) (string-append "a" (number->string (var-counter t))))
        ((comp? t) (cond ((type:abs? t) (string-append "(" (print-list (type:abs-args t) type:print " ") " -> " (type:print (type:abs-res t)) ")"))
                         ((type:pair? t) (string-append "(" (type:print (type:pair-fst t)) "." (type:print (type:pair-scd t)) ")"))
                         ((type:list? t) (string-append "[" (type:print (type:list-inner t)) "]"))
                         ((type:num? t) "Number")
                         ((type:bool? t) "Boolean")
                         ((type:str? t) "String")
                         ((type:void? t) "Void")
                         (else (error "unknown comp type" t))))
        ((ptype? t) (string-append "\\-/" (set:print (ptype-links t) type:print) "." (type:print (ptype-mtype t))))
        (else (error "unknown type" type))))

(define (type:unify ts1 ts2)
  (define (make-trace)
    (string-append "{" (print-list (zip ts1 ts2) (lambda (pair) (string-append (type:print (car pair)) " => " (type:print (cdr pair)))) ", ") "}"))
  (define (subst type key value)
    (type:map-var type (lambda (var) (if (eq? key var) value var))))
  (define (step lefts rights vars values)
    (cond ((and (null? lefts) (null? rights)) (dico:add-all (dico:make) vars values))
          ((or (null? lefts) (null? rights)) (error "the type lists doesn't have the same length" ts1 ts2))
          ((let ((l (car lefts))
                 (r (car rights))
                 (ls (cdr lefts))
                 (rs (cdr rights)))
             (cond ((var? l) (cond ((eq? l r) (step ls rs vars values))
                                   ((type:contain-var? r l) (raise (string-append "Type error: infinite type on: " (make-trace) ".\n")))
                                   (else (step (map (lambda (t) (subst t l r)) ls)
                                               (map (lambda (t) (subst t l r)) rs)
                                               (cons l vars)
                                               (cons r (map (lambda (t) (subst t l r)) values))))))
                   ((var? r) (step (cons r ls) (cons l rs) vars values))
                   ((and (comp? l) (comp? r)) (if (and (eq? (comp-tag l) (comp-tag r)) (equal? (length (comp-types l)) (length (comp-types r))))
                                                  (step (append (comp-types l) ls) (append (comp-types r) rs) vars values)
                                                  (raise (string-append "Type error: unification failed on: " (make-trace) ".\n"))))
                   (else (error "invalid types" l r)))))))
  (step ts1 ts2 '() '()))