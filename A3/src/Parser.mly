%{

        (*we have decided to make the parse tree and give this as input
         * to the main which will also take eval input and perform the
         * given requirments like 
         *                      1. type checking
         *                      2. evaluation                           *)
        open Lexing
        open Printf
        open Tree
%}
        /*the token declaration zone*/
        %token NEWLINE                            /*the newline token*/
        %token CLOSED_PARENTHESIS OPEN_PARENTHESIS
        %token OPEN_BRACKET CLOSE_BRACKET
        %token ASSIGN COMMA SEMICOL
        %token COUNT ROWCOUNT COLCOUNT
        %token MAX ROWMAX COLMAX
        %token MIN ROWMIN COLMIN
        %token SUM ROWSUM COLSUM
        %token AVG ROWAVG COLAVG
        %token ADD MULT SUBT DIV
        %token <float> NUM
        %token <int array> INDICES
        %token <int array> RANGES
        %start main
        %type <Tree.node*Tree.node> main

        %%
        /*the grammer part is here*/
        const: NUM {Zeroary (CONST ($1))}|   CLOSED_PARENTHESIS NUM OPEN_PARENTHESIS {Zeroary (CONST ($2))}
        range: RANGES {Zeroary (RANGE ($1:range))} |  CLOSED_PARENTHESIS RANGES OPEN_PARENTHESIS {Zeroary (RANGE ($2:range))}
        indice:  INDICES {Zeroary (INDICE ($1:index))}|  CLOSED_PARENTHESIS INDICES OPEN_PARENTHESIS {Zeroary (INDICE ($2:index))}
        main2: exp {$1} | CLOSED_PARENTHESIS exp OPEN_PARENTHESIS {$2}
        main: main2 NEWLINE {$1}
        exp: indice ASSIGN ufunct SEMICOL {$1,$3} | indice ASSIGN bfunct SEMICOL {$1,$3}
        ufunct:
                | CLOSED_PARENTHESIS ufunct OPEN_PARENTHESIS {$2}
                | MAX range  {Unary (MAX,$2)}
                | ROWMAX range  {Unary (ROWMAX,$2)}
                | COLMAX range  {Unary (COLMAX,$2)}
                | MIN range  {Unary (MIN,$2)}
                | ROWMIN range {Unary (ROWMIN,$2)}
                | COLMIN range  {Unary (COLMIN,$2)}
                | COUNT range  {Unary (COUNT,$2)}
                | COLCOUNT range  {Unary (COLCOUNT,$2)}
                | ROWCOUNT range  {Unary (ROWCOUNT,$2)}
                | AVG range  {Unary (AVG,$2)}
                | ROWAVG range {Unary (ROWAVG,$2)}
                | COLAVG range {Unary (COLAVG,$2)}
                | SUM range  {Unary (SUM,$2)}
                | ROWSUM range {Unary (ROWSUM,$2)}
                | COLSUM range {Unary (COLSUM,$2)}
       
        bfunct:
                | SUBT indice range {Binary (SUBT,$2,$3)}
                | SUBT range indice {Binary (SUBT,$2,$3)}
                | SUBT range const  {Binary (SUBT,$2,$3)}
                | SUBT const range  {Binary (SUBT,$2,$3)}
                | SUBT range range  {Binary (SUBT,$2,$3)}
                | ADD indice range  {Binary (ADD,$2,$3)}
                | ADD range indice {Binary (ADD,$2,$3)}
                | ADD range const  {Binary (ADD,$2,$3)}
                | ADD const range  {Binary (ADD,$2,$3)}
                | ADD range range  {Binary (ADD,$2,$3)}
                | MULT indice range {Binary (MULT,$2,$3)}
                | MULT range indice {Binary (MULT,$2,$3)}
                | MULT range const  {Binary (MULT,$2,$3)}
                | MULT const range  {Binary (MULT,$2,$3)}
                | MULT range range  {Binary (MULT,$2,$3)}
                | DIV indice range {Binary (DIV,$2,$3)}
                | DIV range indice {Binary (DIV,$2,$3)}
                | DIV range const  {Binary (DIV,$2,$3)}
                | DIV const range  {Binary (DIV,$2,$3)}
                | DIV range range  {Binary (DIV,$2,$3)}
