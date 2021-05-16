%{
        (*we are going to make interpreter for sheets
        first we will start with simple expresions and work on them
        so in this section we are going to define the tokes as*)
        open Lexing
        open Printf
        open T2


%}

        /* the token declaration zone*/
        %token NEWLINE
        %token OPEN_PARENTHESIS CLOSE_PARENTHESIS
        %token PLUS MINUS MUL DIV
        %token <float> NUM
        %token exp
        %left PLUS MINUS
        %left DIV MUL
        %start main
        %type <T2.node> main

        /*the grammer rules are as follows*/
        %%
        main : exp NEWLINE {$1};
        exp: NUM {print_string ("num "^(string_of_float $1)^"\n"); Uniary ( N $1)  }
        | exp PLUS exp { print_string "plus\n";Binary ($1,Plus,$3 ) }
        | exp MINUS exp {print_string "minus\n";Binary ($1,Minus,$3 )}
        | exp MUL exp {print_string "mul\n";  Binary ($1,Mul,$3 )}
        | exp DIV exp {print_string "div\n";Binary ($1,Div,$3 )}
        | OPEN_PARENTHESIS exp CLOSE_PARENTHESIS {print_string "para\n ";$2};
        %%

 print_string "i am here\n";;
