(load "compiler.scm")

(define compile-scheme-file
  (lambda (source target)
   (let* ((pe-lst (map (lambda (ex) (annotate-tc 
                                      (pe->lex-pe 
                                        (box-set 
                                          (remove-applic-lambda-nil 
                                            (eliminate-nested-defines 
                                              (parse ex))))))) (string->sexpr (string->list (file->string source)))))
          (const-table (make-const-table pe-lst))
          (code-gen-lst (map code-gen pe-lst const-table)))
      (string->file (string-append prologue (apply string-append code-gen-lst) epilogue) target)                          
  )
))      
  


(define string->sexpr
    (lambda (str)
      (<Sexpr> str (lambda(match un-match) 
                           (if (null? un-match) 
                             (list match) 
                             (cons match (string->sexpr un-match)))) 
               (lambda(x) `(failed ,x)))
    ))


(define prologue
   "
    #include <stdio.h>
    #include <stdlib.h>
    
    /* change to 0 for no debug info to be printed: */
    #define DO_SHOW 1
    
    #include \"cisc.h\"
    
    int main()
    {
      START_MACHINE;
    
      JUMP(CONTINUE);
  
    #include \"char.lib\"
    #include \"io.lib\"
    #include \"math.lib\"
    #include \"string.lib\"
    #include \"system.lib\"

    #define SOB_VOID 1
	#define SOB_NIL 2
	#define SOB_FALSE 3
	#define SOB_TRUE 5

    CONTINUE:\n"
)

(define epilogue
   "
    STOP_MACHINE;

    return 0;
    }"
)





(define string->file
  (lambda (str out-file)
    (let ((out-port (open-output-file out-file))
          (str-lst (string->list str)))
          (letrec ((run
     (lambda (str-lst)
      (if (null? str-lst)
          (close-output-port out-port)
          (begin
            (write-char (car str-lst) out-port)
            ;(write-char (car str-lst))
            (run (cdr str-lst)))))))
          (run str-lst)
      ))
))

          
          

(define file->string
  (lambda (in-file)
    (let ((in-port (open-input-file in-file)))
      (letrec ((run
                 (lambda ()
                   (let ((ch (read-char in-port)))
                     (if (eof-object? ch)
                       (begin
                         (close-input-port in-port)
                         '())
                       (cons ch (run)))))))
        (list->string
          (run))))))


(define find-consts-in-pe
	(lambda (pe)
		(cond ((const-expr? pe) pe)
			  ((if-expr? pe) `(,(find-consts-in-pe (get-if-test pe)) ,(find-consts-in-pe (get-if-dit pe)) 
			  	             ,(find-consts-in-pe (get-if-dif pe))))      
	          ((applic-expr? pe) `(,(find-consts-in-pe (get-applic-operator pe)) 
	          	                ,@(map find-consts-in-pe (get-applic-operands pe))))
	          ((tc-applic-expr? pe) `(,(find-consts-in-pe (get-applic-operator pe)) 
	          	                   ,@(map find-consts-in-pe (get-applic-operands pe))))
	          ((seq-expr? pe) (map find-consts-in-pe (get-seq-list pe)))
	          ((lambda-simple-expr? pe) (find-consts-in-pe (get-lambda-simple-body pe)))
	          ((lambda-opt-expr? pe) (find-consts-in-pe (get-lambda-opt-body pe)))
	          ((lambda-var-expr? pe) (find-consts-in-pe (get-lambda-var-body pe)))
	          ((or-expr? pe) (map find-consts-in-pe (get-or-body pe)))
	          ((define-expr? pe) `(,(find-consts-in-pe (get-def-var pe)) ,(find-consts-in-pe (get-def-val pe))))
	          ((set-expr? pe) `(,(find-consts-in-pe (get-set-var pe)) ,(find-consts-in-pe (get-set-val pe))))
	          (else '()))
	)) 


;----expression for debuging-----
(define scheme-lst
 `(,(parse `(if 33 44 55)) ,(parse '(lambda (a) x y (+ 3 4) '(a b c (x y))))))

(define scheme-int
  `(,(parse ''3)))
;----expression for debuging-----

(define flatten-const-list 
  (lambda (list)
	   (cond ((null? list) list)
	         ((list? (car list)) (if (or (null? (car list)) (not (equal? (caar list) 'const)))
                                        (append (flatten-const-list (car list)) (flatten-const-list (cdr list)))
                                        (cons (car list) (flatten-const-list (cdr list)))))
	         (else (cons (car list) (flatten-const-list (cdr list)))))))
	          
(define in-list?
  (lambda (lst elem)
    (cond ((null? lst) #f)
          ((equal? (car lst) elem) #t)
          (else (in-list? (cdr lst) elem)))))

(define remove-from-list
  (lambda (lst elem)
    (cond ((null? lst) (cons elem '()))
          ((equal? (car lst) elem) (remove-from-list (cdr lst) elem))
          (else (cons (car lst) (remove-from-list (cdr lst) elem))))))


(define remove-double
   (lambda (c-lst)
     (cond ((null? c-lst) c-lst)
           ((in-list? (cdr c-lst) (car c-lst)) (remove-double (cdr c-lst)))
           (else (cons (car c-lst) (remove-double (cdr c-lst)))))))

;;tags constants with type and returns a list of constant and his tagged sub-constants
;;@param : c - the constant
; (define find-sub-and-tag
;   (lambda (c)
;     (cond ((char? c) `(char ,c))
;           ((integer? c) `(integer ,c))
;           ((number? c) `(fraction ,c))
;           ((string? c) `(string ,c))
;           ((pair? c) `((pair ,c) (find-sub-and-tag (car c)) (find-sub-and-tag (cdr c))))


(define find-sub-and-tag
  (lambda (c)
    (cond ((char? c) `(,c char))
          ((integer? c) `(,c integer 1))
          (else `(other ,c)))))
         

(define find-sub-consts
  (lambda (c-table)
    (if (null? c-table)
        c-table
        (cons (find-sub-and-tag (get-const-val (car c-table))) (find-sub-consts (cdr c-table)))))) 


(define make-const-table
	(lambda (pe-lst)
		(remove-double (find-sub-consts (flatten-const-list (map find-consts-in-pe pe-lst))))
    ;(remove-double (flatten-const-list (map find-consts-in-pe pe-lst)))
		))


(define run
  (lambda (expr)
    (annotate-tc 
      (pe->lex-pe 
        (box-set 
          (remove-applic-lambda-nil 
            (eliminate-nested-defines(parse (string->sexpr (string->list expr))))))))))



(define code-gen
  (lambda (pe)
    (cond ((if-expr? pe) (code-gen-if pe))
          ((pvar-expr? pe) "not yet implemented\n")
          ((bvar-expr? pe) "not yet implemented\n")
          ((fvar-expr? pe) "not yet implemented\n")
          ((const-expr? pe) "not yet implemented\n")
          ((applic-expr? pe) "not yet implemented\n")
          ((tc-applic-expr? pe) "not yet implemented\n")
          ((seq-expr? pe) "not yet implemented\n")
          ((lambda-simple-expr? pe) "not yet implemented\n")
          ((lambda-opt-expr? pe) "not yet implemented\n")
          ((lambda-var-expr? pe) "not yet implemented\n")
          ((or-expr? pe) "not yet implemented\n")
          ((define-expr? pe) "not yet implemented\n")
          ((set-expr? pe) "not yet implemented\n")
          (else "not yet implemented\n"))   
  ))

(define label-generator
    (let ((n 0))
      (lambda (type)
        (set! n (+ n 1))
        (string-append "L" type (number->string n)))
      ))


(define code-gen-if
  (lambda (if-pe)
    (let ((test (get-if-test if-pe))
          (dit (get-if-dit if-pe))
          (dif (get-if-dif if-pe))
          (dif_label (label-generator "_if3_dif_"))
          (exit_label (label-generator "_if3_exit_")))

      (string-append
        "\n"
        (code-gen test)
        "CMP(R0,IMM(SOB_FALSE));\n"
        "JUMP_EQ("dif_label");\n"
        (code-gen dit)
        "JUMP_EQ("exit_label");\n"
        dif_label":\n"
        (code-gen dif)
        exit_label":\n"
        )
      )
  ))

