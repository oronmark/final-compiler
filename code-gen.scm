(load "compiler.scm")

(define compile-scheme-file
  (lambda (source target)
   (let* ((pe-lst (map (lambda (ex) (annotate-tc 
                                      (pe->lex-pe 
                                        (box-set 
                                          (remove-applic-lambda-nil 
                                            (eliminate-nested-defines 
                                              (parse ex))))))) (string->sexpr (string->list (file->string source)))))
          (code-gen-lst (map code-gen pe-lst)))
      (string->file (string-append prologue (apply string-append code-gen-lst) epilogue) target)                          
  )
))      
  

; (code-gen-str (apply string-append pe-lst)))
;(string->file (string-append prologue code-gen-str epilogue) target)  
;(string-append prologue (code-gen (file->string source)) epilogue)


(define string->sexpr
    (lambda(str)
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

 	 #define SOB_VOID 1
	 #define SOB_NIL 2
	 #define SOB_FALSE 3
	 #define SOB_TRUE 5
   
    #include \"char.lib\"
    #include \"io.lib\"
    #include \"math.lib\"
    #include \"string.lib\"
    #include \"system.lib\"

    "
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

