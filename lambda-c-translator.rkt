#lang at-exp racket

(require rackunit)

;; AST for lambda calculus. Pretty much a direct translation from the concrete
;; syntax, except each λ term is also given a unique number to identify it,
;; as well as all the identifiers that it is closed over.
(struct NumC (num) #:transparent)
(struct IdC (id) #:transparent)
;; A lambda's environment contains each identifier in scope inside the lambda,
;; not including the lambda's parameter. These are in order of most-recently
;; declared to least recently declared. Shadowed outer bindings are not included
(struct LamC (index env arg body) #:transparent)
(struct PrintlnC (arg) #:transparent)
(struct AppC (func arg) #:transparent)
(struct BinopC (op l r) #:transparent)
(struct Ifleq0C (c t e) #:transparent)

;; Top-level transformation. Takes an s-expression lambda calculus term and
;; returns the equivalent C program as a string.
(define (transform lc)
  (transform-ast (parse lc)))

;; Transforms the AST into the equivalent C program
(define (transform-ast ast)
  @~a{
#include <stdio.h>
#include <stdlib.h>

typedef union value value;
typedef struct closure closure;

struct closure {
   void *env;
   value (*func)(void *, value);
};

union value {
   int num;
   closure func;
};

value call(value clos, value arg) {
   return (*clos.func.func)(clos.func.env, arg);
}

value println(value arg) {
   printf("%d\n", arg.num);
   return arg;
}

@(transform-lams (extract-lams ast))

int main() {
   @(transform-exp ast '_);
   return 0;
}
})

;; Takes a list of each lambda term, and produces the corresponding stuctures,
;; and functions in C. These are put in reverse order in terms of indices to
;; avoid having to declare prototypes.
(define (transform-lams lams)
  (string-join (map transform-lam (reverse lams)) "\n"))

;; Produces the environment structure, initialization function, and body
;; function for a given lambda term
(define (transform-lam lam)
  (match-define (LamC i env arg body) lam)
  @~a{
      
typedef struct {
@(env-struct-fields env)             
} lam@|i|_env;

value lam@|i|(void *v_env, value @arg) {
   lam@|i|_env *env = (lam@|i|_env *) v_env;
   return @(transform-exp body arg);
}

value init_lam@|i|(@(init-lam-param-list env)) {
   closure clos;
   lam@|i|_env *env = malloc(sizeof(lam@|i|_env));
@(init-lam-env-assignments env)
   clos.env = env;
   clos.func = lam@|i|;
   return (value) clos;
}

})

;; Produces the list of params an init_lam function expects
(define (init-lam-param-list env)
  (string-join (map init-lam-param env) ", "))

;; Just annotates an id with the value type
(define (init-lam-param param)
  @~a{value @param})

;; Produces the list of assignments into the environment needed for a lam_init
(define (init-lam-env-assignments env)
  (string-join (map init-lam-env-assignment env) "\n"))

;; Produces the assignment of an id into an env for a lam_init function
(define (init-lam-env-assignment id)
  @~a{   env->@id = @id;})

;; Transforms an expression into the corresponding C expression. λ terms are
;; transformed into calls to the corresponing lam_init function to create the
;; closure
(define (transform-exp ast arg)
  (define (rec ast) (transform-exp ast arg))
  (match ast
    [(NumC num) @~a{((value) @num)}]
    [(IdC id) (if (symbol=? arg id) (~a id) (id-from-env id))]
    [(LamC i env _ _) @~a{init_lam@|i|(@(init-lam-arg-list env))}]
    [(PrintlnC p-arg) @~a{println(@(rec p-arg))}]
    [(AppC func a-arg) @~a{call(@(rec func), @(rec a-arg))}]
    [(BinopC op l r) @~a{((value) (@(rec l).num @op @(rec r).num))}]
    [(Ifleq0C c t e) @~a{((@(rec c).num <= 0) ? @(rec t) : @(rec e))}]))

;; Creates the arguments needed for a lam_init function. The first argument
;; is the current lamda's argument, while the rest come from the environment
(define (init-lam-arg-list env)
  (string-join
   (match env
     [(list) empty]
     [(cons f r) (cons (~a f) (map id-from-env r))])
   ", "))

;; Accesses and id from the environment
(define (id-from-env id)
  @~a{env->@id})

;; Produces a list of the fields needed for a lamda's environment structure
(define (env-struct-fields env)
  (string-join (map env-struct-field env) "\n"))

;; Produces a declaration of an id as a struct field
(define (env-struct-field id)
  @~a{   value @id;})

;; Returns a list of each LamC term in the AST, in order of increasing index
(define (extract-lams ast)
  (match ast
    [(NumC _) empty]
    [(IdC _) empty]
    [(LamC _ _ _ body) (cons ast (extract-lams body))]
    [(PrintlnC arg) (extract-lams arg)]
    [(AppC func arg) (append (extract-lams func) (extract-lams arg))]
    [(BinopC _ l r) (append (extract-lams l) (extract-lams r))]
    [(Ifleq0C c t e) (append (extract-lams c) (extract-lams t) (extract-lams e))]))

;; Parses a lambda calculus term to an AST
(define (parse lc)
  (define index 1)
  (define (actual-parse lc env)
    (define (rec sub) (actual-parse sub env))
    (match lc
      [(? real? num) (NumC num)]
      [(? symbol? s) (IdC s)]
      [(list 'λ (list (? symbol? arg)) body)
       (LamC index env arg
             (begin
               (set! index (+ index 1))
               (actual-parse body (cons arg (remove arg env)))))]
      [(list 'println arg) (PrintlnC (rec arg))]
      [(list func arg) (AppC (rec func) (rec arg))]
      [(list (? binop? op) l r) (BinopC op (rec l) (rec r))]
      [(list 'ifleq0 c t e) (Ifleq0C (rec c) (rec t) (rec e))]
      [else (error "invalid term" lc)]))
  (actual-parse lc empty))

;; Predicate for valid binary operations
(define (binop? sym)
  (member sym '(+ *)))

;; Read lambda calculus term from command line arg and write output to output.c
(define source-file
  (command-line #:args (filename) filename))

(with-input-from-file source-file
  (λ ()
    (with-output-to-file "output.c"
      (λ ()
        (display (transform (read))))))
  #:mode 'text)