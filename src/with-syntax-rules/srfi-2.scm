;
; Syntax:
;   LAND* (CLAWS) BODY
;
; where CLAWS is a list of expressions or bindings: 
; CLAWS ::= '() | (cons CLAW CLAWS)
; Every element of the CLAWS list, a CLAW, must be one of the following:
; (VARIABLE EXPRESSION)
; or
; (EXPRESSION)
; or
; BOUND-VARIABLE
; These CLAWS are evaluated in the strict left-to-right order. For each
; CLAW, the EXPRESSION part is evaluated first (or BOUND-VARIABLE is looked up).
; If the result is #f, LAND* immediately returns #f, thus disregarding the rest
; of the CLAWS and the BODY. If the EXPRESSION evaluates to not-#f, and
; the CLAW is of the form
; (VARIABLE EXPRESSION)
; the EXPRESSION's value is bound to a freshly made VARIABLE. The VARIABLE is
; available for _the rest_ of the CLAWS, and the BODY. As usual, all
; VARIABLEs must be unique (like in let*).
;
; Thus LAND* is a sort of cross-breed between LET* and AND.
;
; Denotation semantics:
;
; Eval[ (LAND* (CLAW1 ...) BODY), Env] =
; EvalClaw[ CLAW1, Env ] andalso 
;   Eval[ (LAND* ( ...) BODY), ExtClawEnv[ CLAW1, Env]]
;
; Eval[ (LAND* (CLAW) ), Env] = EvalClaw[ CLAW, Env ]
; Eval[ (LAND* () FORM1 ...), Env] = Eval[ (BEGIN FORM1 ...), Env ]
; Eval[ (LAND* () ), Env] = #t
;
; EvalClaw[ BOUND-VARIABLE, Env ] = Eval[ BOUND-VARIABLE, Env ]
; EvalClaw[ (EXPRESSION), Env ] = Eval[ EXPRESSION, Env ]
; EvalClaw[ (VARIABLE EXPRESSION), Env ] = Eval[ EXPRESSION, Env ]
;
; ExtClawEnv[ BOUND-VARIABLE, Env ] = Env
; ExtClawEnv[ (EXPRESSION), Env ] = EnvAfterEval[ EXPRESSION, Env ]
; ExtClawEnv[ (VARIABLE EXPRESSION), Env ] = 
; ExtendEnv[ EnvAfterEval[ EXPRESSION, Env ],
;      VARIABLE boundto Eval[ EXPRESSION, Env ]]
;
;
; If one has a Scheme interpreter written in Prolog/ML/Haskell, he can
; implement the above semantics right away. Within Scheme, it is trivial to
; code LAND* with R4RS "define-syntax". Alas, Gambit does not have this
; facility. So this implementation uses macros instead.
;
; The following LAND* macro will convert a LAND* expression into a "tree" of
; AND and LET expressions. For example,
; (LAND* ((my-list (compute-list)) ((not (null? my-list))))
; (do-something my-list))
; is transformed into
; (and (let ((my-list (compute-list)))
; (and my-list  (not (null? my-list)) (begin (do-something my-list)))))
;
; I must admit the LAND* macro is written in a pathetic anti-functional style.
; To my excuse, the macro's goal is a syntactic transformation of source
; code, that is, performing a re-writing. IMHO, rewriting kind of suggests
; mutating.
;
; Sample applications:
;
; The following piece of code (from my treap package) 
;   (let ((new-root (node:dispatch-on-key root key ...)))
;      (if new-root (set! root new-root)))
; could be elegantly re-written as
;   (land* ((new-root (node:dispatch-on-key root key ...)))
;   (set! root new-root))
;
; A very common application of land* is looking up a value
; associated with a given key in an assoc list, returning #f in case of a
; look-up failure:
;
;   ; Standard implementation
; (define (look-up key alist)
;   (let ((found-assoc (assq key alist)))
; (and found-assoc (cdr found-assoc))))
;
;     ; A more elegant solution
; (define (look-up key alist)
;   (cdr (or (assq key alist) '(#f . #f))))
;
;     ; An implementation which is just as graceful as the latter
;   ; and just as efficient as the former:
; (define (look-up key alist)
;   (land* ((x (assq key alist))) (cdr x)))
;
; Generalized cond:
;
; (or
;   (land* (bindings-cond1) body1)
;   (land* (bindings-cond2) body2)
;   (begin else-clause))
;
; Unlike => (cond's send), LAND* applies beyond cond. LAND* can also be used
; to generalize cond, as => is limited to sending of only a single value;
; LAND* allows as many bindings as necessary (which are performed in sequence)
;
; (or
;  (land* ((c (read-char)) ((not (eof-object? c))))
; (string-set! some-str i c) (++! i))
;  (begin (do-process-eof)))
;
; Another concept LAND* is reminiscent of is programming with guards:
; a LAND* form can be considered a sequence of _guarded_ expressions.
; In a regular program, forms may produce results, bind them to variables
; and let other forms use these results. LAND* differs in that it checks
; to make sure that every produced result "makes sense" (that is, not an #f).
; The first "failure" triggers the guard and aborts the rest of the
; sequence (which presumably would not make any sense to execute anyway).
;
; $Id: vland-gambit.scm,v 1.1 1998/12/28 23:54:29 srfimgr Exp $


(define-macro (and-let* claws . body)
  (let* ((new-vars '()) (result (cons 'and '())) (growth-point result))

      ; We need a way to report a syntax error
      ; the following is how Gambit compiler does it...
    (##define-macro (ct-error-syntax msg . args)
      `(##signal '##signal.syntax-error #t ,msg ,@args))

    (define (andjoin! clause)
      (let ((prev-point growth-point) (clause-cell (cons clause '())))
        (set-cdr! growth-point clause-cell)
        (set! growth-point clause-cell)))

    (if (not (list? claws))
      (ct-error-syntax "bindings must be a list " bindings))
    (for-each 
      (lambda (claw)
        (cond
          ((symbol? claw) ; BOUND-VARIABLE form
            (andjoin! claw))
          ((and (pair? claw) (null? (cdr claw))) ; (EXPRESSION) form
            (andjoin! (car claw)))
            ; (VARIABLE EXPRESSION) form
          ((and (pair? claw) (symbol? (car claw))
              (pair? (cdr claw)) (null? (cddr claw)))
            (let* ((var (car claw)) (var-cell (cons var '())))
              (if (memq var new-vars)
                (ct-error-syntax "duplicate variable " var " in the bindings"))
              (set! new-vars (cons var new-vars))
              (set-cdr! growth-point `((let (,claw) (and . ,var-cell))))
              (set! growth-point var-cell)))
          (else
            (ct-error-syntax "An ill-formed binding in a syntactic form land* " 
              claw))
        ))
      claws)
    (if (not (null? body))
      (andjoin! `(begin ,@body)))
    result))