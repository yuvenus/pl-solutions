;; LET-interp.scm
;; Venus Yu

(load "helpers.scm")

;; ================ Parser Definitions ==================================

;; This defines the translation from the concrete syntax to the abstract syntax.
;; Whenever  you add to or modify the concrete or abstract syntax the specification
;; below must be updated.

(define the-grammar
  '((program                                   ;; <Program> ::=
     (expression)                              ;;   Concrete    <Expression>
     a-prog)                                   ;;   Abstract    (a-prog exp)

    (program                                   ;; <Program> ::=
     ("def!" identifier "=" expression)        ;;   Concrete       def! <Identifier> = <Expression>
     def-prog)                                 ;;   Abstract      (def-prog var exp)

    (expression                                ;; <Expression> ::=
     (number)                                  ;;   Concrete       <Number>
     const-exp)                                ;;   Abstract       (const-exp num)

    (expression                                ;; <Expression> ::=
     ("-(" expression "," expression ")")      ;;   Concrete       -(<Expression>,<Expression>)
     diff-exp)                                 ;;   Abstract       (diff-exp exp1 exp2)

    (expression                                ;; <Expression> ::=
     ("+(" expression "," expression ")")      ;;   Concrete       +(<Expression>,<Expression>)
     add-exp)                                  ;;   Abstract       (add-exp exp1 exp2)

    (expression                                ;; <Expression> ::=
     ("*(" expression "," expression ")")      ;;   Concrete       *(<Expression>,<Expression>)
     mult-exp)                                 ;;   Abstract       (mult-exp exp1 exp2)

    (expression                                ;; <Expression> ::=
     ("/(" expression "," expression ")")      ;;   Concrete       /(<Expression>,<Expression>)
     div-exp)                                  ;;   Abstract       (div-exp exp1 exp2)

    (expression                                ;; <Expression> ::=
     ("<(" expression "," expression ")")      ;;   Concrete       <(<Expression>,<Expression>)
     less-exp)                                 ;;   Abstract       (less-exp exp1 exp2)

    (expression                                ;; <Expression> ::=
     ("<=(" expression "," expression ")")     ;;   Concrete       <=(<Expression>,<Expression>)
     lesseq-exp)                               ;;   Abstract       (lesseq-exp exp1 exp2)

    (expression                                ;; <Expression> ::=
     ("=(" expression "," expression ")")      ;;   Concrete       =(<Expression>,<Expression>)
     eq-exp)                                   ;;   Abstract       (eq-exp exp1 exp2)

    (expression                                ;; <Expression> ::=
     ("zero?(" expression ")")                 ;;   Concrete       zero?(<Expression>)
     zero?-exp)                                ;;   Abstract       (zero?-exp exp)

    (expression                                             ;; <Expression> ::=
     ("if" expression "then" expression "else" expression)  ;;   Concrete       if <Expression> then <Expression> else <Expression>
     if-exp)                                                ;;   Abstract       (if-exp exp1 exp2 exp3)

    (expression                                ;; <Expression> ::=
     ("&("expression "," expression ")")       ;;   Concrete       &(<Expression>,<Expression>)
     and-exp)                                  ;;   Abstract       (and-exp exp1 exp2)

    (expression                                ;; <Expression> ::=
     ("|(" expression "," expression ")")      ;;   Concrete       |(<Expression>,<Expression>)
     or-exp)                                   ;;   Abstract       (or-exp exp1 exp2)

    (expression                                ;; <Expression> ::=
     ("!(" expression ")")                     ;;   Concrete       !(<Expression>)
     not-exp)                                  ;;   Abstract       (not-exp exp)

    (expression                                             ;; var
     (identifier)
     var-exp)

    (expression
     ("let" identifier "=" expression "in" expression)
     let-exp)

    (expression                                ;; <Expression> ::=
     ("#true")                                 ;;   Concrete    #true
     const-true-exp)                           ;;   Abstract    (const-true-exp)

    (expression                                ;; <Expression> ::=
     ("#false")                                ;;   Concrete    #false
     const-false-exp)                          ;;   Abstract    (const-false-exp)

    ))

;; Sets up the parser using the above concrete <-> abstract grammars.
;; Defines a function call parse that takes a string in the concrete
;; syntax and returns the corresponding abstract syntax tree. You must
;; have defined the-grammar first.
(load "lex-scan-parse.scm")


;; =============== Environment Definition =============================

;; This is an implementation of the var-val pair list representation
;; of an environment, we wrote earlier.  I translated the
;; representation into a define-datatype so we get the constructors
;; and type checking predicate for free, and can use cases to process.

(define-datatype environment environment?
  (empty-env)                   ;; (empty-env) gives an empty environment
  (extend-env                   ;; (extend-env var val env) extends the environment
   (var symbol?)
   (val expval?)
   (env environment?))
  )

;; (apply-env env target-var) s to figure out the maping of target-var
;; in the environment env.
(define apply-env ; Env x Var -> SType
  (lambda (env target-var)
    (cases environment env
	   [empty-env () (raise-exception 'apply-env "No binding for ~s" target-var)]
	   [extend-env (var val env*)
		  (cond
			  [(equal? var target-var) val]
			  [else (apply-env env* target-var)])])))

(define make-init-env
  (lambda ()
    (extend-env 'pi (num-val 3.14159)
      (extend-env 'e (num-val 2.71828) (empty-env)))))

(define display-env
  (lambda (env)
    (cases environment env
      [empty-env () (display "]")]
      [extend-env (var val env*)
        (cond
          [(equal? env* (empty-env))  (printf "~a = ~a" var (expval->string val)) (display-env env*)]
          [else (printf "~a = ~a, " var (expval->string val)) (display-env env*)])])))

;; ==================== Expressed Values ==================================

(define-datatype expval expval?
  (unit-val)
  (num-val
   (num number?))
  (bool-val
   (b boolean?))
  )

(define expval->num
  (lambda (ev)
    (cases expval ev
	   [num-val (num) num]
     [bool-val (b) (if b 1 0)]
	   [else (raise-exception 'expval->num "Expressed value is not a number: ~s" ev)])))

(define expval->bool
  (lambda (ev)
    (cases expval ev
	   [bool-val (b) b]
     [num-val (num) (if (= num 0) #f #t)]
	   [else (raise-exception 'expval->bool "Expressed value is not a Boolean: ~s" ev)])))

(define expval->string
  (lambda (ev)
    (cases expval ev
	   [bool-val (b) (if b "#true" "#false")]
	   [num-val (num) (number->string num)]
     [unit-val () "unit-val"]
	   )))


;; ==================== Evaluater =========================================
(define value-of-prog
  (lambda (prog env)
    (cases program prog
	   [a-prog (exp) (value-of-exp exp env)]
     [def-prog (var exp) (cons (unit-val) (extend-env var (value-of-exp exp env) env))]
	   [else (raise-exception 'value-of-prog "Abstract syntax case not implemented: ~s" (car prog))])))

;; still need to fix divide 0 to have the proper error message!
(define value-of-exp
  (lambda (exp env)
    (cases expression exp
	   [const-exp (num) (num-val num)]
	   [diff-exp (rand1 rand2) (num-val (- (expval->num (value-of-exp rand1 env)) (expval->num (value-of-exp rand2 env))))]
     [add-exp (rand1 rand2) (num-val (+ (expval->num (value-of-exp rand1 env)) (expval->num (value-of-exp rand2 env))))]
     [mult-exp (rand1 rand2) (num-val (* (expval->num (value-of-exp rand1 env)) (expval->num (value-of-exp rand2 env))))]
     [div-exp (rand1 rand2)
       (cond
         [(= 0 (expval->num (value-of-exp rand2 env))) (raise-exception 'value-of-exp "Can't divide by zero!" (car exp))]
         [else (num-val (/ (expval->num (value-of-exp rand1 env)) (expval->num (value-of-exp rand2 env))))])]
     [less-exp (rand1 rand2) (bool-val (< (expval->num (value-of-exp rand1 env)) (expval->num (value-of-exp rand2 env))))]
     [lesseq-exp (rand1 rand2) (bool-val (<= (expval->num (value-of-exp rand1 env)) (expval->num (value-of-exp rand2 env))))]
     [eq-exp (rand1 rand2) (bool-val (= (expval->num (value-of-exp rand1 env)) (expval->num (value-of-exp rand2 env))))]
     [and-exp (exp1 exp2) (bool-val (and (expval->bool (value-of-exp exp1 env)) (expval->bool (value-of-exp exp2 env))))]
     [or-exp (exp1 exp2) (bool-val (or (expval->bool (value-of-exp exp1 env)) (expval->bool (value-of-exp exp2 env))))]
     [not-exp (exp1) (bool-val (not (expval->bool (value-of-exp exp1 env))))]
     [zero?-exp (exp1) (bool-val (= (expval->num (value-of-exp exp1 env)) 0))]
	   [if-exp (exp1 exp2 exp3)
		   (let
		       ([val1 (expval->bool (value-of-exp exp1 env))])
		     (if val1 (value-of-exp exp2 env) (value-of-exp exp3 env)))]
	   [var-exp (var) (apply-env env var)]
	   [let-exp (var exp1 exp2)
		    (let
			    ([val1 (value-of-exp exp1 env)])
		        (value-of-exp exp2 (extend-env var val1 env)))]
     [const-true-exp () (bool-val #t)]
     [const-false-exp () (bool-val #f)]
	   [else (raise-exception 'value-of-exp "Abstract syntax case not implemented: ~s" (car exp))])))


;; =================== Interpreter =========================================
;; (start) -- Starts the interpreter.
(define start
  (lambda ()
    (begin
      (display "\n    +-+-+-+-+-+-+  +-+  +-+-+  +-+-+  +-+-+-+-+-+-+-+-+-+-+-+
    w e l c o m e  t o  t h e  l e t  i n t e r p r e t e r !
    +-+-+-+-+-+-+  +-+  +-+-+  +-+-+  +-+-+-+-+-+-+-+-+-+-+-+\n\n")
      (read-eval-print (make-init-env)))))

;; (get-input-string) -- Reads a line from the interactive input
;; port.  Ignores zero length strings.
(define get-input-string
  (lambda ()
    (let ([str (get-line (current-input-port))])
      (if (= (string-length str) 0)
	  (get-input-string)
	  str))))

;; (read-eval-print) -- Main read, eval, and print loop.
(define read-eval-print
  (lambda (env)
    ;; Display an interpreter prompt
    (display "==> ")
    ;; Read a line user input
    (let ([concrete-code (get-input-string)])
      (cond
        [(equal? concrete-code "!quit")
	        (display "Goodbye!")  ;; Quit if 'quit entered.
	        (newline)]
        [(equal? concrete-code "!debug1")
          (trace value-of-prog value-of-exp)
          (read-eval-print env)]
        [(equal? concrete-code "!debug2")
          (trace value-of-prog value-of-exp expval->num expval->bool expval->string)
          (read-eval-print env)]
        [(equal? concrete-code "!debug0")
          (untrace value-of-prog value-of-exp expval->num expval->bool expval->string)
          (read-eval-print env)]
        [(equal? concrete-code "!env")
          (display "[")
          (display-env env)
          (newline)
          (read-eval-print env)
          ]
        [(equal? concrete-code "!reset-env")
          (newline)
          (read-eval-print (make-init-env (empty-env)))]
        [else
	       (guard
	        (ex
	           [else
	            (display "PARSE ERROR: \n")
	             (display-exception ex)])
	 ;; Parse code, eval expression, and print result.
	 (let
	     ([abstract-code (parse concrete-code)])
	   (guard
	    (ex
	     [else
	      (display "RUNTIME ERROR: \n")
	      (display-exception ex)])
      (cond
        [(equal? (car abstract-code) 'def-prog) (display (expval->string (car (value-of-prog abstract-code env))))
                                                 (newline)
                                                 (read-eval-print (cdr (value-of-prog abstract-code env)))]
        [else (display (expval->string (value-of-prog abstract-code env)))
                       (newline)
                       (read-eval-print env)])

      )))]))))
