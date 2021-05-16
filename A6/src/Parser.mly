%{      
         open Printf;;
         open Eval;;             (*the first code block*)
         open String;;           (*the tokenizer for our file*)
         (*some helpful function*)
         exception Eof;;
%}
        
        /*the parser block in this block we will define tokens*/
        %token <string> UPPER_STRING                     /*Variable*/
        %token <string> LOWER_STRING                     /*function or constant*/
        %token <string> FUNAME
        %token COMMA
        %token ENDLINE                     
        %token ASSIGN
        %token CP
        %token EOF
        %token OP
        %start main_file
        %start main_shell                             /*start state*/
        %type <Eval.clause list> main_file
        %type <Eval.af list> main_shell                         /*type of state*/
        /*the production form*/
        /*the production rules*/
%%
        term: 
                UPPER_STRING {Var ($1:variable)}
              | LOWER_STRING {Const ($1:constant)}
              | OP term CP {$2} ;

        term_seq: 
                term {[$1]}
              | af {[Funct $1]}
              | term COMMA term_seq {$1::$3}
              | af COMMA term_seq {(Funct $1)::$3};

        af:
                FUNAME  CP {((Eval.f $1),0),[]}
              | OP af CP {$2}
              | FUNAME term_seq CP {((f $1) , List.length $2),$2};

        af_seq: 
                af {[$1]}
              | af COMMA af_seq {$1::$3};

        rule:   af ASSIGN af_seq {Rule ($1,$3)};

        main_file:  
                rule ENDLINE main_file {$1::$3}
              | af ENDLINE main_file {(Fact $1)::$3}
              | EOF {[]}

        main_shell:
            af_seq ENDLINE {$1}
            | EOF {[]};


                       
%%

(*the last block of code
  af   ENDLINE main {Fact $1}
              | rule ENDLINE main {$1}



              *)



