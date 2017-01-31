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
         (code-gen-lst (map (lambda (ex) (code-gen ex const-table)) pe-lst)))
      (string->file (string-append (prologue const-table) (apply string-append code-gen-lst) epilogue) target)                          
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
   (lambda (c-table)

(string-append
   "
    #include <stdio.h>
    #include <stdlib.h>
    
    /* change to 0 for no debug info to be printed: */
    #define DO_SHOW 1
    
    #include \"cisc.h\"
    #include \"debug_macros.h\"
    
    int main()
    {
      START_MACHINE;
    
      JUMP(CONTINUE);
  
    #include \"char.lib\"
    #include \"io.lib\"
    #include \"math.lib\"
    #include \"string.lib\"
    #include \"system.lib\"
    #include \"scheme.lib\"

    CONTINUE:

    #define SOB_VOID 1
	  #define SOB_NIL 2
	  #define SOB_FALSE 3
	  #define SOB_TRUE 5

/*-------------const table-------------*/"

  	(generate-const-code c-table)

"/*-------------const table-------------*/\n"

  
    )))




 

(define epilogue

  (string-append
    "\n"
    "PUSH(R0);\n"
    "CALL(WRITE_SOB);\n"
    "DROP(IMM(1));\n\n"

    "CALL(NEWLINE);\n"

    "STOP_MACHINE;\n\n"

    "return 0;\n"
    "}"


    )
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

(define get-c-table-elem-val
  (lambda (expr)
    (car expr)))

(define get-c-table-elem-tag
  (lambda (expr)
    (cadr expr)))

(define get-c-table-elem-address
  (lambda (expr)
    (caddr expr)))

(define generate-const-code
	(lambda (c-table)
		(cond ((null? c-table) "")
          ((get-c-table-elem-tag (car c-table)) (string-append 
                                                      "\n"
                                                      "PUSH(IMM("(number->string (get-c-table-elem-val (car c-table)))"));\n"
                                                      "CALL(MAKE_SOB_INTEGER);\n"
                                                      "DROP(1);\n\n"  (generate-const-code (cdr c-table))))
          (else "this is char \n\n"))

))


(define find-sub-and-tag
  (lambda (c address)
    (cond ((char? c) `(,c char))
          ((integer? c) `(,c integer ,address))
          (else `(other ,c)))))
         

(define find-sub-consts
  (lambda (c-table address)
    (if (null? c-table)
        c-table
        (cons (find-sub-and-tag (get-const-val (car c-table)) address) (find-sub-consts (cdr c-table) (+ address 2))))))


(define make-const-table
	(lambda (pe-lst)
		(remove-double (find-sub-consts (flatten-const-list (map find-consts-in-pe pe-lst)) 1))
    ;(remove-double (flatten-const-list (map find-consts-in-pe pe-lst)))
		))


(define run
  (lambda (expr)
    (annotate-tc 
      (pe->lex-pe 
        (box-set 
          (remove-applic-lambda-nil 
            (eliminate-nested-defines(parse (string->sexpr (string->list expr))))))))))

(define try
  (lambda (target)
    (compile-scheme-file "const.scm" target)))



(define code-gen
  (lambda (pe c-table)
    (cond ((if-expr? pe) (code-gen-if pe c-table))
          ((pvar-expr? pe) "not yet implemented\n")
          ((bvar-expr? pe) "not yet implemented\n")
          ((fvar-expr? pe) "not yet implemented\n")
          ((const-expr? pe) (code-gen-const pe c-table))
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


(define get-const-address
  (lambda (c-table const-pe)
    (let ((first (car c-table)))
      (if (equal? (get-c-table-elem-val first) (get-const-val const-pe))
          (get-c-table-elem-address first)
          (get-const-address (cdr c-table) const-pe)))
    ))

(define code-gen-const
  (lambda (const-pe c-table)
    (let ((const-val (get-const-val const-pe)))
      (string-append "MOV(R0,IMM("(number->string (get-const-address c-table const-pe)) "));\n"))
    ))
                    

(define code-gen-if
  (lambda (if-pe c-table)
    (let ((test (get-if-test if-pe))
          (dit (get-if-dit if-pe))
          (dif (get-if-dif if-pe))
          (dif_label (label-generator "_if3_dif_"))
          (exit_label (label-generator "_if3_exit_")))

      (string-append
        "\n"
        (code-gen test c-table)
        "CMP(R0,IMM(SOB_FALSE));\n"
        "JUMP_EQ("dif_label");\n"
        (code-gen dit c-table)
        "JUMP_EQ("exit_label");\n"
        dif_label":\n"
        (code-gen dif c-table)
        exit_label":\n"
        )
      )
  ))
