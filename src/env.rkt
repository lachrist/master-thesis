#lang racket

(require "dico.rkt" "type.rkt" "set.rkt" "value.rkt" "locus.rkt")

(provide env:make-synt
         env:make-infer
         env:make-inter
         env:push-frame
         env:pop-frame
         env:lookup
         env:project
         env:add
         env:add-all
         env:set!
         env:add!
         env:add-all!)

(struct env (dicos) #:transparent)

(define (env:lookup e id)
  (define (perform dicos)
    (if (null? dicos)
        (raise (string-append "Syntax error: unbound identifier " (symbol->string id) ".\n"))
        (dico:lookup (car dicos) id (lambda () (perform (cdr dicos))))))
  (perform (env-dicos e)))

(define (env:push-frame e)
  (env (cons (dico:make) (env-dicos e))))

(define (env:pop-frame e)
  (env (cdr (env-dicos e))))

(define (env:project e locus)
  (env (map (lambda (dico) (dico:map-value dico (lambda (type) (locus:project locus type)))) (env-dicos e))))

(define (env:set! e id value)
  (define (loop dicos)
    (cond ((null? dicos) (raise (string-append "Syntax error:unbound identifier " (symbol->string id) ".\n")))
          ((dico:contain? (car dicos) id) (dico:add! (car dicos) id value))
          (else (loop (cdr dicos)))))
  (loop (env-dicos e)))

(define (env:add e id value)
  (dico:add (car (env-dicos e)) id value))

(define (env:add-all e ids values)
  (env (cons (dico:add-all (car (env-dicos e)) ids values) (cdr (env-dicos e)))))

(define (env:add! e id value)
  (env (cons (dico:add! (car (env-dicos e)) id value) (cdr (env-dicos e)))))

(define (env:add-all! e ids values)
  (dico:add-all! (car (env-dicos e)) ids values))

(define (env:make-synt)
  (let* ((primitives (list '+ '- '* '/ 'and 'or 'concat 'pair 'pair? 'fst 'scd 'cons 'list? 'car 'cdr 'null 'null? 'equal? '< '> 'string? 'number? 'boolean? 'read-line 'display 'boolean->string 'number->string 'string-boolean? 'string-number? 'string->boolean 'string->number))
         (bindings (map (const '()) primitives)))
    (env (list (dico:add-all (dico:make) primitives bindings)))))

(define *var1* (type:make-var!))
(define *var2* (type:make-var!))

(define (env:make-infer)
  (define mono-set (set:from-list (list *var1*)))
  (define duo-set (set:from-list (list *var1* *var2*)))
  (define g (dico:make))
  (set! g (dico:add g '+                                  (type:make-abs (list (type:make-num) (type:make-num)) (type:make-num))))
  (set! g (dico:add g '-                                  (type:make-abs (list (type:make-num) (type:make-num)) (type:make-num))))
  (set! g (dico:add g '*                                  (type:make-abs (list (type:make-num) (type:make-num)) (type:make-num))))
  (set! g (dico:add g '/                                  (type:make-abs (list (type:make-num) (type:make-num)) (type:make-num))))
  (set! g (dico:add g 'and                                (type:make-abs (list (type:make-bool) (type:make-bool)) (type:make-bool))))
  (set! g (dico:add g 'or                                 (type:make-abs (list (type:make-bool) (type:make-bool)) (type:make-bool))))
  (set! g (dico:add g 'concat                             (type:make-abs (list (type:make-str) (type:make-str)) (type:make-str))))
  (set! g (dico:add g 'boolean->string                    (type:make-abs (list (type:make-bool)) (type:make-str))))
  (set! g (dico:add g 'number->string                     (type:make-abs (list (type:make-num)) (type:make-str))))
  (set! g (dico:add g 'string-boolean?                    (type:make-abs (list (type:make-str)) (type:make-bool))))
  (set! g (dico:add g 'string-number?                     (type:make-abs (list (type:make-str)) (type:make-bool))))
  (set! g (dico:add g 'string->number                     (type:make-abs (list (type:make-str)) (type:make-num))))
  (set! g (dico:add g 'string->boolean                    (type:make-abs (list (type:make-str)) (type:make-bool))))
  (set! g (dico:add g 'pair     (type:make-ptype duo-set  (type:make-abs (list *var1* *var2*) (type:make-pair *var1* *var2*)))))
  (set! g (dico:add g 'pair?    (type:make-ptype mono-set (type:make-abs (list *var1*) (type:make-bool)))))
  (set! g (dico:add g 'fst      (type:make-ptype duo-set  (type:make-abs (list (type:make-pair *var1* *var2*)) *var1*))))
  (set! g (dico:add g 'scd      (type:make-ptype duo-set  (type:make-abs (list (type:make-pair *var1* *var2*)) *var2*))))
  (set! g (dico:add g 'cons     (type:make-ptype mono-set (type:make-abs (list *var1* (type:make-list *var1*)) (type:make-list *var1*)))))
  (set! g (dico:add g 'list?    (type:make-ptype mono-set (type:make-abs (list *var1*) (type:make-bool)))))
  (set! g (dico:add g 'car      (type:make-ptype mono-set (type:make-abs (list (type:make-list *var1*)) *var1*))))
  (set! g (dico:add g 'cdr      (type:make-ptype mono-set (type:make-abs (list (type:make-list *var1*)) (type:make-list *var1*)))))
  (set! g (dico:add g 'null     (type:make-ptype mono-set (type:make-list *var1*))))
  (set! g (dico:add g 'null?    (type:make-ptype mono-set (type:make-abs (list *var1*) (type:make-bool)))))
  (set! g (dico:add g 'equal?   (type:make-ptype duo-set  (type:make-abs (list *var1* *var2*) (type:make-bool)))))
  (set! g (dico:add g '<                                  (type:make-abs (list (type:make-num) (type:make-num)) (type:make-bool))))
  (set! g (dico:add g '>                                  (type:make-abs (list (type:make-num) (type:make-num)) (type:make-bool))))
  (set! g (dico:add g 'string?  (type:make-ptype mono-set (type:make-abs (list *var1*) (type:make-bool)))))
  (set! g (dico:add g 'number?  (type:make-ptype mono-set (type:make-abs (list *var1*) (type:make-bool)))))
  (set! g (dico:add g 'boolean? (type:make-ptype mono-set (type:make-abs (list *var1*) (type:make-bool)))))
  (set! g (dico:add g 'read-line                          (type:make-abs (list) (type:make-str))))
  (set! g (dico:add g 'display  (type:make-ptype mono-set (type:make-abs (list *var1*) (type:make-void)))))
  (env (list g)))

(define (env:make-inter)
  (define g (dico:make))
  (set! g (dico:add g '+               (value:make-prim value:plus)))
  (set! g (dico:add g '-               (value:make-prim value:minus)))
  (set! g (dico:add g '*               (value:make-prim value:mult)))
  (set! g (dico:add g '/               (value:make-prim value:div)))
  (set! g (dico:add g 'and             (value:make-prim value:and)))
  (set! g (dico:add g 'or              (value:make-prim value:or)))
  (set! g (dico:add g 'concat          (value:make-prim value:concat)))
  (set! g (dico:add g 'boolean->string (value:make-prim value:bool->str)))
  (set! g (dico:add g 'number->string  (value:make-prim value:num->str)))
  (set! g (dico:add g 'string-boolean? (value:make-prim value:str-bool?)))
  (set! g (dico:add g 'string-number?  (value:make-prim value:str-num?)))
  (set! g (dico:add g 'string->boolean (value:make-prim value:str->bool)))
  (set! g (dico:add g 'string->number  (value:make-prim value:str->num)))
  (set! g (dico:add g 'pair            (value:make-prim value:make-pair)))
  (set! g (dico:add g 'fst             (value:make-prim value:fst)))
  (set! g (dico:add g 'scd             (value:make-prim value:scd)))
  (set! g (dico:add g 'cons            (value:make-prim value:make-list)))
  (set! g (dico:add g 'car             (value:make-prim value:head)))
  (set! g (dico:add g 'cdr             (value:make-prim value:tail)))
  (set! g (dico:add g 'null            (value:get-null)))
  (set! g (dico:add g 'null?           (value:make-prim value:null?)))
  (set! g (dico:add g 'equal?          (value:make-prim value:equal?)))
  (set! g (dico:add g '<               (value:make-prim value:lesser?)))
  (set! g (dico:add g '>               (value:make-prim value:greater?)))
  (set! g (dico:add g 'string?         (value:make-prim value:str?)))
  (set! g (dico:add g 'number?         (value:make-prim value:num?)))
  (set! g (dico:add g 'boolean?        (value:make-prim value:bool?)))
  (set! g (dico:add g 'read-line       (value:make-prim value:read-line)))
  (set! g (dico:add g 'display         (value:make-prim value:display)))
  (env (list g)))