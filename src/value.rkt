#lang racket

(provide value:make-const
         value:num?
         value:plus
         value:minus
         value:mult
         value:div
         value:greater?
         value:lesser?
         value:bool?
         value:bool-bool
         value:and
         value:or
         value:str?
         value:concat
         value:read-line
         value:bool->str
         value:num->str
         value:str-bool?
         value:str-num?
         value:str->num
         value:str->bool
         value:make-pair
         value:pair?
         value:fst
         value:scd
         value:make-list
         value:list?
         value:head
         value:tail
         value:make-prim
         value:prim?
         value:prim-apply
         value:make-comp
         value:comp?
         value:comp-env
         value:comp-params
         value:comp-body
         value:make-poly
         value:poly?
         value:poly-dico
         value:poly-comp
         value:get-null
         value:null?
         value:false?
         value:get-void
         value:display
         value:equal?
         value:print)

(struct value () #:transparent)
(struct bool value (bool) #:transparent)
(struct num value (num) #:transparent)
(struct str value (str) #:transparent)
(struct pair value (fst scd) #:transparent)
(struct list value (head tail) #:transparent)
(struct prim value (proc) #:transparent) 
(struct comp value (env params body) #:transparent)
(struct poly value (dico comp) #:transparent)

;; Value wrapper: returns only values

(define (value:make-const bytes)
  (cond ((string? bytes) (str bytes))
        ((boolean? bytes) (bool bytes))
        ((number? bytes) (num bytes))
        (else (error "unknown constant"))))

(define (value:str? a) (bool (str? a)))
(define (value:concat a b) (str (string-append (str-str a) (str-str b))))
(define (value:read-line) (str (read-line)))
(define (value:bool->str a) (str (if (bool-bool a) "#t" "#f")))
(define (value:num->str a) (str (number->string (num-num a))))
(define (value:str-bool? a) (bool (or (equal? (str-str a) "#t") (equal? (str-str a) "#f"))))
(define (value:str-num? a) (bool (let ((n (string->number (str-str a))))
                                      (if (equal? n #f)
                                          #f
                                          #t))))
(define (value:str->num a) (num (let ((n (string->number (str-str a))))
                                  (if (equal? n #f)
                                      (raise (string-append "Runtime error: cannot parse " (str-str a) " as a number."))
                                      n))))
(define (value:str->bool a) (bool (cond ((equal? (str-str a) "#t") #t)
                                       ((equal? (str-str a) "#f") #f)
                                       (else (raise "Runtime error: cannot parse " (str-str a) " as a boolean.")))))

(define (value:bool? a) (bool (bool? a)))
(define (value:and a b) (bool (and (bool-bool a) (bool-bool b))))
(define (value:or a b) (bool (or (bool-bool a) (bool-bool b))))
(define value:bool-bool bool-bool)

(define (value:num? a) (bool (num? a)))
(define (value:plus a b) (num (+ (num-num a) (num-num b))))
(define (value:minus a b) (num (- (num-num a) (num-num b))))
(define (value:mult a b) (num (* (num-num a) (num-num b))))
(define (value:div a b) (if (equal? (num-num b) 0)
                            (raise "Runtime error: division by zero.\n")
                            (num (/ (num-num a) (num-num b)))))
(define (value:greater? a b) (num (> (num-num a) (num-num b))))
(define (value:lesser? a b) (num (< (num-num a) (num-num b))))

(define value:make-pair pair)
(define (value:pair? a) (bool (pair? a)))
(define value:fst pair-fst)
(define value:scd pair-scd)

(define *null* (value))
(define (value:get-null) *null*)
(define (value:null? a) (bool (eq? a *null*)))

(define *void* (value))
(define (value:get-void) *void*)

(define value:make-list list)
(define (value:list? a) (bool (list? a)))
(define (value:head arg)
  (if (eq? arg *null*)
      (raise "Runtime error: null pointer exception on car.\n")
      (list-head arg)))
(define (value:tail arg)
  (if (eq? arg *null*)
      (raise "Runtime error:null pointer exception on cdr.\n")
      (list-tail arg)))

;; Break the value wrapper, may return other stuff than value structure

(define (value:false? v) (and (bool? v) (not (bool-bool v))))

(define value:make-prim prim)
(define value:prim? prim?)
(define (value:prim-apply prim args) (apply (prim-proc prim) args))

(define value:make-comp comp)
(define value:comp? comp?)
(define value:comp-env comp-env)
(define value:comp-params comp-params)
(define value:comp-body comp-body)

(define value:make-poly poly)
(define value:poly? poly?)
(define value:poly-dico poly-dico)
(define value:poly-comp poly-comp)

(define (value:display v)
  (display (value:print v))
  *void*)
(define (value:equal? a b)
  (cond ((and (num? a) (num? b)) (bool (equal? (num-num a) (num-num b))))
        ((and (bool? a) (bool? b)) (bool (equal? (bool-bool a) (bool-bool b))))
        ((and (str? a) (str? b)) (bool (equal? (str-str a) (str-str b))))
        (else (bool (eq? a b)))))

(define (value:print v)
  (define (print-tail v)
    (if (eq? *null* v)
        ""
        (string-append " " (value:print (list-head v)) (print-tail (list-tail v)))))
  (cond ((str? v) (string-append "\"" (str-str v) "\""))
        ((bool? v) (if (bool-bool v) "#t" "#f"))
        ((num? v) (number->string (num-num v)))
        ((pair? v) (string-append "(" (value:print (pair-fst v)) "." (value:print (pair-scd v)) ")"))
        ((list? v) (string-append "(" (value:print (list-head v)) (print-tail (list-tail v)) ")"))
        ((prim? v) "#primitive-procedure")
        ((comp? v) "#compound-procedure")
        ((poly? v) "#polymorphically-wrapped-procedure")
        ((eq? v *null*) "#null")
        ((eq? v *void*) "")
        (else (error "unknown value" v))))