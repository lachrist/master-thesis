#lang racket

(provide inter:perform)

(require "env.rkt" "locus.rkt" "expr.rkt" "value.rkt")

(struct idiom (locus value))
(struct retard (var marker ass pred cons alt))

(define (inter:perform exprs)
  (map idiom-value (inter-all exprs (env:make-inter) (locus:universe) #f)))

(define (inter-all exprs env locus unsafe?)
  (map (lambda (expr)
         (define i (inter expr env locus unsafe?))
         (set! locus (idiom-locus i))
         i)
       exprs))

(define (inter expr env locus unsafe?)
  (cond ((expr:const? expr) (inter-const expr env locus unsafe?))
        ((expr:id? expr) (inter-id expr env locus unsafe?))
        ((expr:lid? expr) (inter-lid expr env locus unsafe?))
        ((expr:app? expr) (inter-app expr env locus unsafe?))
        ((expr:abs? expr) (inter-abs expr env locus unsafe?))
        ((expr:assign? expr) (inter-assign expr env locus unsafe?))
        ((expr:sif? expr) (inter-sif expr env locus unsafe?))
        ((expr:ldif? expr) (inter-ldif expr env locus unsafe?))
        ((expr:def? expr) (inter-def expr env locus unsafe?))
        ((expr:lvif? expr) (inter-lvif expr env locus unsafe?))
        ((expr:unsafe? expr) (inter-unsafe expr env locus unsafe?))
        (else (error "invalid expr" expr))))

(define (inter-const const env locus unsafe?)
  (idiom locus (value:make-const (expr:const-value const))))

(define (inter-id id env locus unsafe?)
  (idiom locus (env:lookup env (expr:id-value id))))

(define (inter-lid lid env locus unsafe?)
  (define bind (env:lookup env (expr:lid-value lid)))
  (if (value:comp? bind)
      (idiom locus (value:make-poly (expr:lid-dico lid) bind))
      (idiom locus bind)))

(define (inter-app app env locus unsafe?)
  (define (apply-mono abs locus args)
    (cond ((value:prim? abs) (idiom locus (value:prim-apply abs args)))
          ((value:comp? abs) (last (inter-all (value:comp-body abs) (env:add-all (env:push-frame (value:comp-env abs)) (value:comp-params abs) args) locus unsafe?)))
          (else "unknown monomorphic procedure" abs)))
  (let* ((i (inter (expr:app-abs app) env locus unsafe?))
         (abs (idiom-value i))
         (locus (idiom-locus i))
         (is (inter-all (expr:app-args app) env locus unsafe?))
         (args (map idiom-value is))
         (locus (if (null? is) locus (idiom-locus (last is)))))
    (if (value:poly? abs)
        (let* ((pre-locus (locus:intersect locus (locus:from-dico (value:poly-dico abs)))) ; this intersection should never fail
               (i (apply-mono (value:poly-comp abs) pre-locus args))
               (post-locus (locus:substitute (idiom-locus i) (value:poly-dico abs))))
          (idiom post-locus (idiom-value i)))
        (apply-mono abs locus args))))

(define (inter-abs abs env locus unsafe?)
  (idiom locus (value:make-comp env (expr:abs-params abs) (expr:abs-body abs))))

(define (inter-assign assign env locus unsafe?)
  (define i (inter (expr:assign-bind assign) env locus unsafe?))
  (env:set! env (expr:assign-param assign) (idiom-value i))
  (idiom (idiom-locus i) (value:get-void)))

(define (inter-sif sif env locus unsafe?)
  (let* ((i (inter (expr:sif-pred sif) env locus unsafe?))
         (expr (if (value:false? (idiom-value i)) (expr:sif-alt sif) (expr:sif-cons sif))))
    (inter expr env (idiom-locus i) unsafe?)))

(define (inter-ldif ldif env locus unsafe?)
  (let* ((i (inter (expr:ldif-pred ldif) env locus unsafe?))
         (next-locus (if (value:false? (idiom-value i)) (expr:ldif-alt-locus ldif) (expr:ldif-cons-locus ldif)))
         (locus (if unsafe? (locus:safe-intersect locus next-locus) (locus:intersect locus next-locus)))
         (expr (if (value:false? (idiom-value i)) (expr:ldif-alt ldif) (expr:ldif-cons ldif))))
    (inter expr env locus unsafe?)))

(define (inter-lvif lvif env locus unsafe?)
  (let ((locus (locus:safe-intersect locus (expr:lvif-attempt-locus lvif))))
    (if (null? locus)
        (inter (expr:lvif-alt lvif)
               env
               (locus:intersect locus (expr:lvif-alt-locus lvif))
               unsafe?)
        (inter (expr:lvif-attempt lvif)
               env
               locus
               unsafe?))))

(define (inter-def def env locus unsafe?)
  (if (expr:abs? (expr:def-bind def))
      (let* ((mono-env (env:push-frame env))
             (mono-abs (value:make-comp mono-env (expr:abs-params (expr:def-bind def)) (expr:abs-body (expr:def-bind def)))))
        (env:add! mono-env (expr:def-name def) mono-abs)
        (env:add! env (expr:def-name def) mono-abs)
        (idiom locus (value:get-void)))
      (let ((i (inter (expr:def-bind def) env locus unsafe?)))
        (env:add! env (expr:def-name def) (idiom-value i))
        (idiom (idiom-locus i) (value:get-void)))))

(define (inter-unsafe unsafe env locus unsafe?)
  (inter (expr:unsafe-expr unsafe) env locus #t))
