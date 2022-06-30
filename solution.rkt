#lang plait

(module+ test
  (print-only-errors #t))

;; abstract syntax -------------------------------

(define-type Exp
  (numE [n : Number])
  (varE [x : Symbol])
  (ifzE [b : Exp] [l : Exp] [r : Exp])
  (opbinE [e1 : Exp] [op : Symbol] [e2 : Exp])
  (letE [x : Symbol] [e1 : Exp] [e2 : Exp])
  (funE [name : Symbol] [xs : (Listof Symbol)] [e : Exp])
  (appE [fun : Symbol] [args : (Listof Exp)])
  (defineE [defs : (Listof Exp)] [res : Exp]))

;; parse ----------------------------------------

(define (parse [s : S-Exp]) : Exp
  (cond
    [(s-exp-match? `{define (ANY ...) for ANY} s)
     (defineE (parse-list (s-exp->list (second (s-exp->list s))))
       (parse (fourth (s-exp->list s))))]
    [(s-exp-match? `NUMBER s)
     (numE (s-exp->number s))]
    [(s-exp-match? `SYMBOL s)
     (varE (s-exp->symbol s))]
    [(s-exp-match? `{ifz ANY then ANY else ANY} s)
     (ifzE (parse (second (s-exp->list s)))
           (parse (fourth (s-exp->list s)))
           (parse (list-ref (s-exp->list s) 5)))]
    [(s-exp-match? `{ANY SYMBOL ANY} s)
     (opbinE (parse (first (s-exp->list s)))
             (parse-op (s-exp->symbol (second (s-exp->list s))))
             (parse (third (s-exp->list s))))]
    [(s-exp-match? `{let SYMBOL be ANY in ANY} s)
     (letE (s-exp->symbol (second (s-exp->list s)))
           (parse (fourth (s-exp->list s)))
           (parse (list-ref (s-exp->list s) 5)))]
    [(s-exp-match? `{fun SYMBOL (ANY ...) = ANY} s)
     (funE (s-exp->symbol (second (s-exp->list s)))
           (map (lambda (x) (s-exp->symbol x)) (s-exp->list (third (s-exp->list s))))
           (parse (list-ref (s-exp->list s) 4)))]
    [(s-exp-match? `{SYMBOL (ANY ...)} s)
     (appE (s-exp->symbol (first (s-exp->list s)))
           (parse-list (s-exp->list (second (s-exp->list s)))))]
    [else (error 'parse "Invalid S-expression")]))

(define prim-ops '(+ - * <=))

(define (parse-op [op : Symbol]) : Symbol
  (if (member op prim-ops)
      op 
      (error 'parse "unknown operator")))

(define (parse-list [xs : (Listof S-Exp)]) : (Listof Exp)
  (type-case (Listof S-Exp) xs
    [empty empty]
    [(cons z zs) (cons (parse z) (parse-list zs))]))
  
;; eval -----------------------------------------

;; values

(define-type-alias Value Number)

;; environments

(define-type Binding
  (bind [name : Symbol]
        [val : Value]))

(define-type-alias Env (Listof Binding))

(define mt-env empty)

(define (extend-env [env : Env] [x : Symbol] [v : Value]) : Env
  (cons (bind x v) env))

(define (extend-env-list [env : Env] [xs : (Listof Symbol)] [vs : (Listof Value)]) : Env
  (type-case (Listof Symbol) xs
    [empty env]
    [(cons y ys) (extend-env-list (extend-env env y (first vs)) ys (rest vs))]))

(define (lookup-env [n : Symbol] [env : Env]) : Value
  (type-case (Listof Binding) env
    [empty (error 'lookup "unbound variable")]
    [(cons b rst-env) (cond
                        [(eq? n (bind-name b))
                         (bind-val b)]
                        [else (lookup-env n rst-env)])]))
;; global functions

(define (find-glob-fun [name : Symbol] [fs : (Listof Exp)]) : Exp
  (type-case (Listof Exp) fs
    [empty (error 'find-glob-fun "Couldn't find such function")]
    [(cons x xs) (if (equal? (funE-name x) name)
                     x
                     (find-glob-fun name xs))]))

(define (fun-duplicate? [name : Symbol] [fs : (Listof Exp)]) : Boolean
  (type-case (Listof Exp) fs
    [empty #f]
    [(cons x xs) (if (equal? (funE-name x) name)
                     #t
                     (fun-duplicate? name xs))]))

(define (arg-duplicate? xs)
  (type-case (Listof Symbol) xs
  [empty #f]
  [(cons y ys) (if (member y ys)
                   #t
                   (arg-duplicate? ys))]))

;; evaluation functions

(define (eval-header [e : Exp] [env : Env] [g-fun : (Listof Exp)]) : Value
  (type-case Exp e
    [(defineE defs res)
     (eval-body res mt-env (validate-glob-fun defs env g-fun))]
    [else (error 'eval-header "Invalid expression for header evaluation")]))

(define (eval-body [e : Exp] [env : Env] [g-fun : (Listof Exp)]) : Value
  (type-case Exp e
    [(numE n) n]
    [(varE x)
     (lookup-env x env)]
    [(ifzE b l r)
     (if (= (eval-body b env g-fun) 0) (eval-body l env g-fun) (eval-body r env g-fun))]
    [(opbinE l o r)
     ((op->proc o) (eval-body l env g-fun) (eval-body r env g-fun))]
    [(letE x e1 e2)
     (let ([v1 (eval-body e1 env g-fun)])
       (eval-body e2 (extend-env env x v1) g-fun))]
    [(appE x args) (apply x (map (lambda (x) (eval-body x env g-fun)) args) env g-fun)]
    [else (error 'eval-body "Invalid expresion")]))

(define (op->proc [op : Symbol]) : (Value Value -> Value)
  (cond
    [(equal? '+ op) +]
    [(equal? '- op) -]
    [(equal? '* op) *]
    [(equal? '<= op) (lambda (x y) (if (<= x y) 0 42))]))

(define (apply [fname : Symbol] [args : (Listof Value)] [env : Env] [g-fun : (Listof Exp)]) : Value
  (let ([fun (find-glob-fun fname g-fun)])
    (if (equal? (length (funE-xs fun)) (length args))
        (eval-body (funE-e fun) (extend-env-list mt-env (funE-xs fun) args) g-fun)
        (error 'apply "Invalid number of arguments for a function"))))
      
(define (validate-glob-fun [exps : (Listof Exp)] [env : Env] [g-fun : (Listof Exp)]) : (Listof Exp)
  (type-case (Listof Exp) exps
    [empty g-fun]
    [(cons x xs) (type-case Exp x
                   [(funE name ys e) (if (fun-duplicate? name g-fun)
                                         (error 'validate-glob-fun "Redefinition of an already existing function")
                                         (if (arg-duplicate? ys)
                                             (error 'valude-glob-fun "Multiple instances of the same argument")
                                             (validate-glob-fun xs env (cons x g-fun))))]
                   [else (error 'validate-glob-fun "Not a function definition")])]))

(define (run [s : S-Exp]) : Value
  (eval-header (parse s) mt-env empty))

;; tests ---------------------------------------------

(module+ test
  (test (run `{define
{[fun fact (n) = {ifz n then 1 else {n * {fact ({n - 1})}}}]}
for
{fact (5)}}) 120)
  (test (run `{define
{[fun even (n) = {ifz n then 0 else {odd ({n - 1})}}]
[fun odd (n) = {ifz n then 42 else {even ({n - 1})}}]}
for
{even (1024)}}) 0)
  (test (run `{define
{[fun gcd (m n) = {ifz n
then m
else {ifz {m <= n}
then {gcd (m {n - m})}
else {gcd ({m - n} n)}}}]}
for
{gcd (81 63)}}) 9))