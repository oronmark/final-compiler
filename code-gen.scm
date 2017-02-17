;; very importent : when finishing the assignment, remove CALL(NEWLINE) from epilogue

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
         (code-gen-lst (map (lambda (ex) (code-gen ex const-table -1)) pe-lst)))

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

/*-------------const table-------------*/\n"

  	(generate-const-in-mem (reverse c-table))

"/*-------------const table-------------*/\n\n"

"/*-------------fake frame--------------*/\n\n"
	"PUSH(IMM(0));\n"
	"PUSH(IMM(T_NIL));\n"
	"PUSH(LABEL(END));\n"
	"PUSH(FP);\n"
	"MOV(FP,SP);\n\n"

"/*-------------fake frame--------------*/\n\n"

  
    )))




 

(define epilogue

  (string-append
    "\n"
    "PUSH(R0);\n"
    "CALL(WRITE_SOB);\n"
    "DROP(IMM(1));\n\n"

    "CALL(NEWLINE);\n"

    "END:\n"

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
(define lst1
 `(,(parse `(if 33 44)) ,(parse '(lambda (a) #\a #\g (+ 3 4) '(a b c (x y))))))

(define lst2
	`(,(parse ''(1 2 3))))

(define lst3
	`(,(parse '#(1 2 3 4 "abc" #\x))))

(define lst4
  `(,(parse ''abc)))

(define lst5
	`(,(parse '"abcd")))

(define lst6
	`(,(parse -2/4)))
;----expression for debuging-----

;----helpers for debug-----

(define run
  (lambda (expr)
    (annotate-tc 
      (pe->lex-pe 
        (box-set 
          (remove-applic-lambda-nil 
            (eliminate-nested-defines(parse expr))))))))

(define try
  (lambda (target)
    (compile-scheme-file "program.scm" target)))

;-----helpers for debug----

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

;-------- getters for const table element --------

(define get-pvar-var
	(lambda (pvar-expr)
		(cadr pvar-expr)))

(define get-pvar-minor
	(lambda (pvar-expr)
		(caddr pvar-expr)))

(define get-c-table-elem-val
  (lambda (expr)
    (caddr expr)))

(define get-c-table-char-ascii
	(lambda (expr)
		(cadddr expr)))

(define get-c-table-symbol-rep
	(lambda (expr)
		(cadddr expr)))

(define get-c-table-fraction-rep
	(lambda (expr)
		(cadddr expr)))

(define get-c-table-string-rep
	(lambda (expr)
		(cadddr expr)))

(define get-c-table-pair-rep
	(lambda (expr)
		(cadddr expr)))

(define get-c-table-vector-rep
	(lambda (expr)
		(cadddr expr)))

(define get-c-table-elem-tag
  (lambda (expr)
    (car expr)))


(define get-c-table-elem-address
  (lambda (expr)
    (cadr expr)))
;-------- getters for const table element --------

;;pushes the argument for MAKE_SOB_STRING to stack
;;@param val : represents string in const-table, example : val = (3 (97 98 99)) , string = "abc"
;;@param str-lst : list of ascii characters representing the string
(define push-string
	(lambda (val)
		(let ((len (car val))
			  (str-lst (cadr val)))
		   (string-append 
		   	   "\n"		   	   
			   (letrec  ((run-push (lambda (str-lst)
			   					(if (null? str-lst)
			   						""
			   						(string-append 
			   							"PUSH(IMM("(number->string (car str-lst))"));\n"
			   							(run-push (cdr str-lst)))))))
			      (run-push str-lst)) 
			   	"PUSH(IMM("(number->string len)"));\n"
			   ))))

(define push-pair
	(lambda (val)
		(string-append			
			"PUSH(IMM("(number->string (cadr val))"));\n"
			"PUSH(IMM("(number->string (car val))"));\n")))

(define push-vector
	(lambda (val)
		(push-string val)))

(define push-fraction
	(lambda (val)
		(push-pair val)))
		   							
(define generate-const-in-mem
	(lambda (c-table)
		(string-append
			"\n"
			"CALL(MAKE_SOB_VOID);\n"
			"CALL(MAKE_SOB_NIL);\n"
			
			"PUSH(IMM(0));\n"
			"CALL(MAKE_SOB_BOOL);\n"
			"DROP(IMM(1));\n"

			"PUSH(IMM(1));\n"
			"CALL(MAKE_SOB_BOOL);\n"
			"DROP(IMM(1));\n"

			(generate-const-code c-table))
		)
	)

(define generate-const-code
	(lambda (c-table)

		(if (null? c-table)
			""
			(let ((first (car c-table)))
				(cond ((equal? (get-c-table-elem-tag first) 'integer)
								(string-append 
	                                   "\n"
	                                   "PUSH(IMM("(number->string (get-c-table-elem-val first))"));\n"
	                                   "CALL(MAKE_SOB_INTEGER);\n"
	                                   "DROP(IMM(1));\n\n"  (generate-const-code (cdr c-table))))
				      ((equal? (get-c-table-elem-tag first) 'char)
								(string-append 
	                                   "\n"
	                                   "PUSH(IMM("(number->string (get-c-table-char-ascii first))"));\n"
	                                   "CALL(MAKE_SOB_CHAR);\n"
	                                   "DROP(IMM(1));\n\n"  (generate-const-code (cdr c-table))))
				      ((equal? (get-c-table-elem-tag first) 'string) 
                                          (string-append
												(push-string (get-c-table-string-rep first))
												 "CALL(MAKE_SOB_STRING);\n"
          									 "DROP("(number->string (+ (string-length (get-c-table-elem-val first)) 1))");\n\n"  
          									 (generate-const-code (cdr c-table))))
				       ((equal? (get-c-table-elem-tag first) 'symbol)
									(string-append 
	                                   "\n"
	                                   "PUSH(IMM("(number->string (get-c-table-symbol-rep first))"));\n"
	                                   "CALL(MAKE_SOB_SYMBOL);\n"
	                                   "DROP(IMM(1));\n\n"  (generate-const-code (cdr c-table))))
				       ((equal? (get-c-table-elem-tag first) 'vector) 
                                          (string-append
												(push-string (get-c-table-vector-rep first))
												 "CALL(MAKE_SOB_VECTOR);\n"
          									     "DROP("(number->string 
          									 	  (+ (length (vector->list(get-c-table-elem-val first))) 1))");\n\n"  
          									 (generate-const-code (cdr c-table))))
				       ((equal? (get-c-table-elem-tag first) 'pair) 
                                          (string-append
												(push-pair (get-c-table-pair-rep first))
												 "CALL(MAKE_SOB_PAIR);\n"
          									 	   "DROP(IMM(2));\n\n" 
          									 (generate-const-code (cdr c-table))))
				       ((equal? (get-c-table-elem-tag first) 'fraction) 
                                          (string-append
												(push-fraction (get-c-table-fraction-rep first))
												 "CALL(MAKE_SOB_FRACTION);\n"
          									 	   "DROP(IMM(2));\n\n" 
          									 (generate-const-code (cdr c-table))))
				      (else ""))))


))
        

(define break-vector
	(lambda (const-expr)
		(let* ((const-val (get-const-val const-expr))
			   (vec-list (vector->list const-val)))
		    `(,const-expr ,@(map (lambda (ex) (find-all-sub-consts-in-const-expr `(const ,ex))) vec-list)))
		))


(define break-pair
	(lambda (const-expr)
		(let ((const-val (get-const-val const-expr)))
		    `(,const-expr
		     (const ,(car const-val)) ,(find-all-sub-consts-in-const-expr `(const ,(car const-val)))
		     (const ,(cdr const-val)) ,(find-all-sub-consts-in-const-expr `(const ,(cdr const-val))))
		    )
		))

(define break-symbol
	(lambda (const-expr)
		(let ((const-val (get-const-val const-expr)))
			`((const ,(symbol->string const-val)) (const ,const-val)))))

(define find-all-sub-consts-in-const-expr
	(lambda (const-expr)
		(let ((const-val (get-const-val const-expr)))
			(cond ((vector? const-val) (break-vector const-expr))
			      ((pair? const-val) (break-pair const-expr))
			      ((symbol? const-val) (break-symbol const-expr))
			      (else const-expr))
			)
		))
		      


(define find-sub-consts
  (lambda (c-table)
    (if (null? c-table)
        c-table
        (cons (find-all-sub-consts-in-const-expr (car c-table)) (find-sub-consts (cdr c-table))))))

(define remove-const-tag
	(lambda (const-list)
		(if (null? const-list)
			const-list
			(cons (get-const-val (car const-list)) (remove-const-tag (cdr const-list))))))

(define remove-bool-void-nil
  (lambda (const-list)
    (cond ((null? const-list) const-list)
          ((or (equal? (car const-list) *void-object*)
               (equal? (car const-list) `(const ()))
               (equal? (car const-list) `(const #t))
               (equal? (car const-list) `(const #f))) (remove-bool-void-nil (cdr const-list)))
          (else (cons (car const-list) (remove-bool-void-nil (cdr const-list)))))))


(define is-subset-in-list
	(lambda (sub set)
		(cond ((null? set) #f)
			  ((equal? sub set) #t)
			  (else (is-subset-in-list sub (cdr set))))
		))

(define is-subset-in-vector
	(lambda (sub set)
		(is-subset-in-list sub (vector->list set))
		))

(define is-subset?
	(lambda (sub set)
		(cond ((list? set) (is-subset-in-list sub set))
			  ((vector? set) (is-subset-in-vector sub set))
			  (else #t))
		))

(define sort-func 
	(lambda (c1 c2)
		(cond ((and (string? c1) (symbol? c2)) #t)
			  ((and (symbol? c1) (string? c2)) #f)
			  (else (if (is-subset? c1 c2)
					 	c1
					 	c2)))))

(define topologica-sort
	(lambda (const-lst)
		(sort sort-func const-lst)))

(define tag-integer
	(lambda (c address)
		`(integer ,address ,c)))

(define tag-fraction
	(lambda (c address)
		`(fraction ,address ,c (,(numerator c) ,(denominator c)))))

(define tag-char
	(lambda (c address)
		`(char  ,address ,c ,(char->integer c))))

(define tag-string
	(lambda (c address)
		`(string ,address ,c (,(string-length c) ,(map char->integer (string->list c))))))

(define tag-vector
	(lambda (c address c-table)
		`(vector ,address ,c (,(length(vector->list c)) ,(map (lambda (ex) (get-const-address c-table ex)) 
																		   (vector->list c))))))
(define tag-symbol
	(lambda (c tagged-list address)
		(let ((symbol-string (symbol->string c)))
			`(symbol ,address ,c ,(get-const-address tagged-list  symbol-string)))))

(define tag-pair
	(lambda (c address c-table)
		`(pair ,address ,c (,(get-const-address c-table (car c)) ,(get-const-address c-table (cdr c))))))


;;returns a list of tupels (tag address explicit-value implicite-value)
(define assign-tag-and-address
	(lambda (const-list tagged-list address)
		(if (null? const-list) 
			tagged-list
			(let ((first (car const-list)))
			    (cond ((integer? first) (assign-tag-and-address (cdr const-list) (cons (tag-integer first address) tagged-list)
			    															     (+ address 2)))
			          ((char? first) (assign-tag-and-address (cdr const-list) (cons (tag-char first address) tagged-list)
			    															     (+ address 2)))
			          ((string? first) (assign-tag-and-address (cdr const-list) (cons (tag-string first address) tagged-list)
			    															     (+ address (string-length first) 2)))
			          ((symbol? first) (assign-tag-and-address (cdr const-list) (cons (tag-symbol first tagged-list address) 
			                                                                     tagged-list) (+ address 2)))	
			          ((vector? first) (assign-tag-and-address (cdr const-list) (cons (tag-vector first address tagged-list) 
			          	                                                         tagged-list)
			    															     (+ address (length(vector->list first)) 2)))
			    	  ((pair? first) (assign-tag-and-address (cdr const-list) (cons (tag-pair first address tagged-list) 
			          	                                                         tagged-list)
			    															     (+ address 3)))		          																   
			    	  ((and (rational? first) (not (integer? first))) (assign-tag-and-address 
			    	  	                                                         (cdr const-list) 
			    	  	                                                         (cons (tag-fraction first address) 
			          	                                                         tagged-list)
			    															     (+ address 3)))														
			    	  (else (assign-tag-and-address (cdr const-list) tagged-list address)))
			    ))))

;;@param const-pe : const without label
(define get-const-address
  (lambda (c-table const-pe)
  	(cond ((equal? `(const ,const-pe) *void-object*) 1)
  		  ((null? const-pe) 2)
  		  ((boolean? const-pe) (if (equal? const-pe #f) 3 5))
	  	   (else 
			  	(if (null? c-table)
			  		#f
				    (let ((first (car c-table)))
				      (if (equal? (get-c-table-elem-val first) const-pe)
				          (get-c-table-elem-address first)
				          (get-const-address (cdr c-table) const-pe))))))
    ))


;;all-const-list is a list of all the constants in the input file /w sub constants
(define make-const-table
	(lambda (pe-lst)
	  (let ((all-const-list (topologica-sort
		  					   (remove-const-tag 
	  	                          (remove-bool-void-nil (remove-double 
		                    	  	(flatten-const-list 
		                    	  		(remove-double 
		                    	  			(find-sub-consts 
		                    	  				(flatten-const-list (map find-consts-in-pe pe-lst)))))))))))
	     (assign-tag-and-address all-const-list '() 7))
		))


(define code-gen
  (lambda (pe c-table env)
    (cond ((if-expr? pe) (code-gen-if pe c-table env))
          ((pvar-expr? pe) (code-gen-pvar pe c-table env))
          ((bvar-expr? pe) (code-gen-bvar pe c-table env))
          ((fvar-expr? pe) "not yet implemented\n")
          ((const-expr? pe) (code-gen-const pe c-table env))
          ((applic-expr? pe) (code-gen-applic pe c-table env))
          ((tc-applic-expr? pe) "not yet implemented\n")
          ((seq-expr? pe) (code-gen-seq pe c-table env))
          ((lambda-simple-expr? pe) (code-gen-lambda-simple pe c-table env))
          ((lambda-opt-expr? pe)(code-gen-lambda-opt pe c-table env))
          ((lambda-var-expr? pe) "not yet implemented\n")
          ((or-expr? pe) (code-gen-or pe c-table env))
          ((define-expr? pe) "not yet implemented\n")
          ((set-expr? pe) "not yet implemented\n")
          (else "error"))   
  ))

(define label-generator
    (let ((n 0))
      (lambda (type)
        (set! n (+ n 1))
        (string-append "L" type (number->string n)))
      ))


(define code-gen-const
  (lambda (const-pe c-table env)
    (let ((const-val (get-const-val const-pe)))
      (string-append "MOV(R0,IMM("(number->string (get-const-address c-table const-val)) "));\n"))
    ))
                    

(define code-gen-if
  (lambda (if-pe c-table env)
    (let ((test (get-if-test if-pe))
          (dit (get-if-dit if-pe))
          (dif (get-if-dif if-pe))
          (dif_label (label-generator "_if3_dif_"))
          (exit_label (label-generator "_if3_exit_")))

      (string-append
      	"\n\n"
		"/////////////////////////////\n" 
		"///code gen: if    - start///\n"
		"/////////////////////////////\n"
		     	
        "\n"
        (code-gen test c-table env)
        "CMP(R0,IMM(SOB_FALSE));\n"
        "JUMP_EQ("dif_label");\n"
        (code-gen dit c-table env)
        "JUMP("exit_label");\n"
        dif_label":\n"
        (code-gen dif c-table env)
        exit_label":\n"
        )
      )
  ))

(define code-gen-seq
	(lambda (seq-pe c-table env)
		(letrec ((run-seq (lambda (elems)
							(if (null? elems)
								""
								(string-append
									(code-gen (car elems) c-table env)
									(run-seq (cdr elems)))))))
		  (string-append 				
		  	    "\n\n"
		  		"/////////////////////////////\n" 
		  	    "///code gen: seq   - start///\n"
		     	"/////////////////////////////\n"
		     	 (run-seq (get-seq-list seq-pe))))))

(define code-gen-or
	(lambda (or-pe c-table env)
		(letrec ((exit-label (label-generator "_exit_or_"))
			     (run-or (lambda (args)
					(if (null? args)
						""
						(string-append
							"\n\n"
		  		            "/////////////////////////////\n" 
		  	                "///code gen: or    - start///\n"
		     	            "/////////////////////////////\n"
		     	
							(code-gen (car args) c-table env)
							"CMP(R0,IMM(SOB_FALSE));\n"
							"JUMP_NE("exit-label");\n"
							(run-or (cdr args)))))))
			(string-append "MOV(R0,IMM(SOB_FALSE));\n" (run-or (get-or-body or-pe)) exit-label ":\n"))))


(define code-gen-lambda-opt
	(lambda (lambda-pe c-table env)
		(let ((body-code (code-gen (get-lambda-opt-body lambda-pe) c-table (+ env 1)))
			  (body-label (label-generator "_lambda_opt_body_"))
			  (copy-old-env-start-label (label-generator "_copy_old_env_start_"))
			  (copy-old-env-end-label (label-generator "_copy_old_env_end_"))
			  (copy-args-start-label (label-generator "_copy_args_start_"))
			  (copy-args-end-label (label-generator "_copy_args_end_"))
			  (exit-label (label-generator "_exit_opt_simple_"))
			  (var-num (length (get-lambda-opt-param lambda-pe)))
			  (make-opt-list-start-label (label-generator "_make_opt_arg_start_"))
			  (make-opt-list-end-label (label-generator "_make_opt_arg_end_"))

			  )

		  (string-append

            "\n\n"
            "/////////////////////////////////////\n" 
            "///code gen: lambda-opt    - start///\n"
            "/////////////////////////////////////\n"

		  	"MOV(R1,FPARG(0));\n\n"  ; get current env in stack

      		"PUSH(IMM("(number->string (+ 1 env))"));\n"
			"CALL(MALLOC);\n" 
 		    "DROP(1);\n" 
 		    "MOV(R2, R0);\n\n"

 		    "MOV(R4,IMM(0));\n" ; pointer to major in old env
 			"MOV(R5,IMM(1));\n\n" ; pointer to major in new env

 			copy-old-env-start-label ":\n"
 				"CMP(R4,IMM("(number->string env)"));\n"
 				"JUMP_GE("copy-old-env-end-label");\n"
 					"MOV(INDD(R2,R5),INDD(R1,R4));\n" 
 	 				"ADD(R4,IMM(1));\n"
                    "ADD(R5,IMM(1));\n" 
                    "JUMP("copy-old-env-start-label");\n"
            copy-old-env-end-label ":\n\n\n"


            "PUSH(FPARG(1));\n"
            "CALL(MALLOC);\n"
            "DROP(1);\n"
            "MOV(R3,R0);\n" ; pointer to list of stack arguments (the minors of the newnenv)
            "MOV(R4,IMM(0));\n\n" ; offset from first argument in stack to current argument

            copy-args-start-label ":\n"
            	"CMP(R4,FPARG(1));\n"
            	"JUMP_EQ(" copy-args-end-label" );\n"
            		"MOV(INDD(R3,R4),FPARG(2 + R4));\n"
            		"ADD(R4,IMM(1));\n"
            		"JUMP(" copy-args-start-label  ");\n"
            copy-args-end-label ":\n\n"

            "MOV(IND(R2), R3);\n\n" ; pointer to new environment (list of majors)

           "PUSH(IMM(3));\n"
           "CALL(MALLOC);\n" 
           "DROP(1);\n\n" 

           "MOV(INDD(R0,0), T_CLOSURE);\n" 
           "MOV(INDD(R0,1), R2);\n"
           "MOV(INDD(R0,2),LABEL("body-label"));\n\n" 
         
           "JUMP("exit-label");\n\n"


;----------------------------------------fix stack------------------
           body-label ":\n"
	           "PUSH(FP);\n"
	           "MOV(FP,SP);\n\n"


			"MOV(R1,SOB_NIL);\n"
			"MOV(R2,FPARG(1)-1);\n\n" ;offset to last optional argument
			
			make-opt-list-start-label ":\n"
			"CMP(R2," (number->string var-num ) " - 1);\n"
			"JUMP_EQ(" make-opt-list-end-label ");\n"
				"PUSH(R1);\n"
				"PUSH(FPARG(2 + R2));\n"
				"CALL(MAKE_SOB_PAIR);\n"
				"DROP(2);\n"
				"SUB(R2,1);\n"
				"MOV(R1,R0);\n"
				"JUMP(" make-opt-list-start-label ");\n"
			make-opt-list-end-label ":\n\n" ; R1 holds the list of optional arguments

			"MOV(FPARG("(number->string var-num) " + 2), R1);\n"
			;"MOV(FPARG(2)," (number->string var-num) "+ 1);\n\n" ;change number of arguments in stack

	        body-code
	        "POP(FP);\n" 
	        "RETURN;\n\n"
;----------------------------------------fix stack------------------
           exit-label ":\n\n"

))))



(define code-gen-lambda-simple
	(lambda (lambda-pe c-table env)
		(let ((body-code (code-gen (get-lambda-simple-body lambda-pe) c-table (+ env 1)))
			  (body-label (label-generator "_lambda_simple_body_"))
			  (copy-old-env-start-label (label-generator "_copy_old_env_start_"))
			  (copy-old-env-end-label (label-generator "_copy_old_env_end_"))
			  (copy-args-start-label (label-generator "_copy_args_start_"))
			  (copy-args-end-label (label-generator "_copy_args_end_"))
			  (exit-label (label-generator "_exit_lambda_simple_")))

		  (string-append

            "\n\n"
            "/////////////////////////////////////\n" 
            "///code gen: lambda-simple - start///\n"
            "/////////////////////////////////////\n"

		  	"MOV(R1,FPARG(0));\n\n"  ; get current env in stack

      		"PUSH(IMM("(number->string (+ 1 env))"));\n"
			"CALL(MALLOC);\n" 
 		    "DROP(1);\n" 
 		    "MOV(R2, R0);\n\n"

 		    "MOV(R4,IMM(0));\n" ; pointer to major in old env
 			"MOV(R5,IMM(1));\n\n" ; pointer to major in new env

 			copy-old-env-start-label ":\n"
 				"CMP(R4,IMM("(number->string env)"));\n"
 				"JUMP_GE("copy-old-env-end-label");\n"
 					"MOV(INDD(R2,R5),INDD(R1,R4));\n" 
 	 				"ADD(R4,IMM(1));\n"
                    "ADD(R5,IMM(1));\n" 
                    "JUMP("copy-old-env-start-label");\n"
            copy-old-env-end-label ":\n\n\n"


            "PUSH(FPARG(1));\n"
            "CALL(MALLOC);\n"
            "DROP(1);\n"
            "MOV(R3,R0);\n" ; pointer to list of stack arguments (the minors of the newnenv)
            "MOV(R4,IMM(0));\n\n" ; offset from first argument in stack to current argument

            copy-args-start-label ":\n"
            	"CMP(R4,FPARG(1));\n"
            	"JUMP_EQ(" copy-args-end-label" );\n"
            		"MOV(INDD(R3,R4),FPARG(2 + R4));\n"
            		"ADD(R4,IMM(1));\n"
            		"JUMP(" copy-args-start-label  ");\n"
            copy-args-end-label ":\n\n"

            "MOV(IND(R2), R3);\n\n" ; pointer to new environment (list of majors)

           "PUSH(IMM(3));\n"
           "CALL(MALLOC);\n" 
           "DROP(1);\n\n" 

           "MOV(INDD(R0,0), T_CLOSURE);\n" 
           "MOV(INDD(R0,1), R2);\n"
           "MOV(INDD(R0,2),LABEL("body-label"));\n\n" 
         
           "JUMP("exit-label");\n\n"

           body-label ":\n"
	           "PUSH(FP);\n"
	           "MOV(FP,SP);\n"
	           body-code
	           "POP(FP);\n" 
	           "RETURN;\n\n"

           exit-label ":\n\n"

))))



(define code-gen-applic
	(lambda (applic-expr c-table env)
		(let* ((proc (get-applic-operator applic-expr))
			  (args (get-applic-operands applic-expr))
			  (arg-num (length args))
			  (args-code-gen-lst (map (lambda (ex) (string-append (code-gen ex c-table env) "\nPUSH(R0)\n")) args))
			  (args-code-gen (apply string-append (reverse args-code-gen-lst))))

		  (string-append
		  	"\n\n"
		  	"/////////////////////////////\n" 
		  	"///code gen: applic- start///\n"
		  	"/////////////////////////////\n"
		  	args-code-gen
		  	"\n"
		  	"PUSH(IMM("(number->string arg-num)"))\n"
		  	(code-gen proc c-table env)
		  	"PUSH(INDD(R0,1))\n"
		  	"CALLA(INDD(R0,2))\n"
		  	"DROP(IMM(1))\n"
		  	"POP(R1)\n"
		  	"DROP(R1)\n")
		
		  )))

(define code-gen-pvar
	(lambda (pvar-expr c-table env)
		(let ((minor (caddr pvar-expr)))
			(string-append 
				"MOV(R0,FPARG("(number->string (+ 2 minor))"))\n"))))

(define code-gen-bvar
	(lambda (bvar-expr get-c-table env)
		(let ((major (caddr bvar-expr))
			  (minor (cadddr bvar-expr)))
		  (string-append
		  	"MOV(R0,FPARG(0));\n"
		  	"MOV(R0,INDD(R0,"(number->string major)"))\n"
		  	"MOV(R0,INDD(R0,"(number->string minor)"))\n"
		  	))))








