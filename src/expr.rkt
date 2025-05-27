#lang racket

(require "locus.rkt" "type.rkt" "misc.rkt" "dico.rkt")

(provide expr:print
         ;; misc
         expr:make-const
         expr:const?
         expr:const-value
         expr:make-id
         expr:id?
         expr:id-value
         expr:make-lid
         expr:lid?
         expr:lid-value
         expr:lid-dico
         expr:make-abs
         expr:abs?
         expr:abs-params
         expr:abs-body
         expr:make-app
         expr:app?
         expr:app-abs
         expr:app-args
         expr:make-assign
         expr:assign?
         expr:assign-param
         expr:assign-bind
         expr:make-def
         expr:def?
         expr:def-name
         expr:def-bind
         expr:make-sif
         expr:sif?
         expr:sif-pred
         expr:sif-cons
         expr:sif-alt
         expr:make-unsafe
         expr:unsafe?
         expr:unsafe-expr
         ;; vif
         expr:make-vif
         expr:vif?
         expr:vif-attempt
         expr:vif-alt
         expr:make-dvif
         expr:dvif?
         expr:dvif-attempt
         expr:dvif-alt
         expr:dvif-env
         expr:dvif-type
         expr:make-lvif
         expr:lvif?
         expr:lvif-attempt-locus
         expr:lvif-attempt
         expr:lvif-alt-locus
         expr:lvif-alt
         ;; dif
         expr:make-dif
         expr:dif?
         expr:dif-pred
         expr:dif-cons
         expr:dif-alt
         expr:make-ddif
         expr:ddif?
         expr:ddif-pred
         expr:ddif-cons
         expr:ddif-alt
         expr:ddif-env
         expr:ddif-type
         expr:make-ldif
         expr:ldif?
         expr:ldif-pred
         expr:ldif-cons-locus
         expr:ldif-cons
         expr:ldif-alt-locus
         expr:ldif-alt
         )

(struct expr () #:transparent)
(struct const expr (value) #:transparent)
(struct id expr (value) #:transparent)
(struct lid expr (value dico) #:transparent)
(struct app expr (abs args) #:transparent)
(struct abs expr (params body) #:transparent)
(struct assign expr (param bind) #:transparent)
(struct def expr (name bind) #:transparent)
(struct unsafe expr (expr) #:transparent)
(struct sif expr (pred cons alt) #:transparent)
(struct dif expr (pred cons alt) #:transparent)
(struct ddif expr (pred cons alt env type) #:transparent)
(struct ldif expr (pred cons-locus cons alt-locus alt) #:transparent)
(struct vif expr (attempt alt) #:transparent)
(struct dvif expr (attempt alt env type) #:transparent)
(struct lvif expr (attempt-locus attempt alt-locus alt) #:transparent)

(define expr:make-id id)
(define expr:id? id?)
(define expr:id-value id-value)

(define expr:make-const const)
(define expr:const? const?)
(define expr:const-value const-value)

(define expr:make-lid lid)
(define expr:lid? lid?)
(define expr:lid-value lid-value)
(define expr:lid-dico lid-dico)

(define expr:make-app app)
(define expr:app? app?)
(define expr:app-abs app-abs)
(define expr:app-args app-args)

(define expr:make-abs abs)
(define expr:abs? abs?)
(define expr:abs-params abs-params)
(define expr:abs-body abs-body)

(define expr:make-assign assign)
(define expr:assign? assign?)
(define expr:assign-param assign-param)
(define expr:assign-bind assign-bind)

(define expr:make-def def)
(define expr:def? def?)
(define expr:def-name def-name)
(define expr:def-bind def-bind)

(define expr:make-unsafe unsafe)
(define expr:unsafe? unsafe?)
(define expr:unsafe-expr unsafe-expr)

(define expr:make-sif sif)
(define expr:sif? sif?)
(define expr:sif-pred sif-pred)
(define expr:sif-cons sif-cons)
(define expr:sif-alt sif-alt)

;; dif = if-dynamic
;; ddid = delayed-if-dynamic
;; ldif = locused-if-dynamic

(define expr:make-dif dif)
(define expr:dif? dif?)
(define expr:dif-pred dif-pred)
(define expr:dif-cons dif-cons)
(define expr:dif-alt dif-alt)

(define expr:make-ddif ddif)
(define expr:ddif? ddif?)
(define expr:ddif-pred ddif-pred)
(define expr:ddif-cons ddif-cons)
(define expr:ddif-alt ddif-alt)
(define expr:ddif-env ddif-env)
(define expr:ddif-type ddif-type)

(define expr:make-ldif ldif)
(define expr:ldif? ldif?)
(define expr:ldif-pred ldif-pred)
(define expr:ldif-cons-locus ldif-cons-locus)
(define expr:ldif-cons ldif-cons)
(define expr:ldif-alt-locus ldif-alt-locus)
(define expr:ldif-alt ldif-alt)

;; vif = if-viable
;; dvid = delayed-if-viable
;; lvif = locused-if-viable

(define expr:make-vif vif)
(define expr:vif? vif?)
(define expr:vif-attempt vif-attempt)
(define expr:vif-alt vif-alt)

(define expr:make-dvif dvif)
(define expr:dvif? dvif?)
(define expr:dvif-attempt dvif-attempt)
(define expr:dvif-alt dvif-alt)
(define expr:dvif-env dvif-env)
(define expr:dvif-type dvif-type)

(define expr:make-lvif lvif)
(define expr:lvif? lvif?)
(define expr:lvif-attempt-locus lvif-attempt-locus)
(define expr:lvif-attempt lvif-attempt)
(define expr:lvif-alt-locus lvif-alt-locus)
(define expr:lvif-alt lvif-alt)

(define (expr:print expr i)
  (define out "")
  (define indents (list i))
  (define current i)
  (define (push!)
    (set! indents (cons current indents)))
  (define (pop!)
    (set! indents (cdr indents)))
  (define (add! str)
    (set! current (+ current (string-length str)))
    (set! out (string-append out str)))
  (define (newline!)
    (set! out (string-append out "\n" (make-string (car indents) #\ )))
    (set! current (car indents)))
  (define (perform! expr)
    (cond ((expr:const? expr) (let ((value (expr:const-value expr)))
                                (add! (cond ((number?  value) (number->string value))
                                            ((string? value) (string-append "\"" value "\""))
                                            ((boolean? value) (if value "#t" "#f"))
                                            (else "unknown constant" value)))))
          ((expr:id? expr) (add! (symbol->string (id-value expr))))
          ((expr:lid? expr) (add! (string-append "[" (symbol->string (expr:lid-value expr)) " " (dico:print (expr:lid-dico expr) type:print) "]")))
          ((expr:abs? expr) (begin (add! "(lambda ")
                                   (push!)
                                   (add! (string-append "(" (print-list (expr:abs-params expr) symbol->string " ") ")"))
                                   (for-each (lambda (expr) (newline!) (perform! expr)) (expr:abs-body expr))
                                   (add! ")")
                                   (pop!)))
          ((expr:app? expr) (begin (add! "(")
                                   (perform! (expr:app-abs expr))
                                   (for-each (lambda (expr) (add! " ") (perform! expr)) (expr:app-args expr))
                                   (add! ")")))
          ((expr:assign? expr) (begin (add! "(set! ")
                                      (add! (string-append (symbol->string (expr:assign-param expr)) " "))
                                      (perform! (expr:assign-bind expr))
                                      (add! ")")))
          ((expr:def? expr) (begin (add! "(define ")
                                   (add! (string-append (symbol->string (expr:def-name expr)) " "))
                                   (perform! (expr:def-bind expr))
                                   (add! ")")))
          ((expr:sif? expr) (begin (add! "(sif ")
                                   (push!)
                                   (perform! (expr:sif-pred expr))
                                   (newline!)
                                   (perform! (expr:sif-cons expr))
                                   (newline!)
                                   (perform! (expr:sif-alt expr))
                                   (add! ")")
                                   (pop!)))
          ((expr:dif? expr) (begin (add! "(dif ")
                                   (push!)
                                   (perform! (expr:dif-pred expr))
                                   (newline!)
                                   (perform! (expr:dif-cons expr))
                                   (newline!)
                                   (perform! (expr:dif-alt expr))
                                   (add! ")")
                                   (pop!)))
          ((expr:ldif? expr) (begin (add! "(dif ")
                                    (push!)
                                    (perform! (expr:ldif-pred expr))
                                    (newline!)        
                                    (add! (string-append "[" (locus:print (expr:ldif-cons-locus expr)) " "))
                                    (perform! (expr:ldif-cons expr))
                                    (add! "]")
                                    (newline!)
                                    (add! (string-append "[" (locus:print (expr:ldif-alt-locus expr)) " "))
                                    (perform! (expr:ldif-alt expr))
                                    (add! "])")
                                    (pop!)))
          ((expr:vif? expr) (begin (add! "(vif ")
                                   (push!)
                                   (perform! (expr:vif-attempt expr))
                                   (newline!)
                                   (perform! (expr:vif-alt expr))
                                   (add! ")")
                                   (pop!)))
          ((expr:lvif? expr) (begin (add! "(vif ")
                                    (push!)     
                                    (add! (string-append "[" (locus:print (expr:lvif-attempt-locus expr)) " "))
                                    (perform! (expr:lvif-attempt expr))
                                    (add! "]")
                                    (newline!)
                                    (add! (string-append "[" (locus:print (expr:lvif-alt-locus expr)) " "))
                                    (perform! (expr:lvif-alt expr))
                                    (add! "])")
                                    (pop!)))
          ((expr:unsafe? expr) (begin (add! "(unsafe ")
                                      (perform! (expr:unsafe-expr expr))
                                      (add! ")")))
          ((expr:dvif? expr) (add! "<dvif>"))
          ((expr:ddif? expr) (add! "<ddif>"))
          (else (error "unknwon expression" expr))))
  (perform! expr)
  (string-append out "\n"))