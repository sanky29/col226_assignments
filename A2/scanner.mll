(*the header seaction 
the lexifier starts working from here*)
{

        (* define some useful variables as*)
        exception Eof

        (*now lets define the type token as*)
        type token =
                 | FLOAT of string                        (*float token eg. 20.034*)
                 | CLOSED_PARENTHESIS                     (*open parenthesis token*)
                 | OPEN_PARENTHESIS                       (*closed parenthesis token*)
                 | CLOSED_BRACKET                         (*open bracket token*)
                 | OPEN_BRACKET                           (*closed parenthesis token*)
                 | COMMA                                  (*the , symbol*)
                 | COLON                                  (*the : symbol*)
                 | INDICES of string                      (*the [i,j] where i and j are natrual number*)
                 | RANGES of string                       (*[i1:i2] where i1 and i2 are indices*)
                 | COUNT                                  (*the overall count*)
                 | ROWCOUNT                               (*row wise count*)
                 | COLCOUNT                               (*the columnwisw count*)
                 | MAX                                    (*the overall max*)
                 | ROWMAX                                 (*row wise max*)
                 | COLMAX                                 (*the columnwisw max*)
                 | SUM                                    (*the overall sum*)
                 | ROWSUM                                 (*row wise sum*)
                 | COLSUM                                 (*the columnwisw sum*)
                 | MIN                                    (*the overall min*)
                 | ROWMIN                                 (*row wise min*)
                 | COLMIN                                 (*the columnwisw min*)
                 | AVG                                    (*the overall AVG*)
                 | ROWAVG                                 (*row wise AVG*)
                 | COLAVG                                 (*the columnwisw AVG*)
                 | ADD                                    (*the binary operator*)
                 | SUBT                                   (*the binary subt operator*)
                 | MULT                                   (*the binary mult operator*)
                 | DIV                                    (*the binary div operator*)
}
        (*the useful regular expresions*)
        let nzdigits = ['1'-'9']                         (*non zero digits {1,2,3,4,5,6,7,8,9} *) 
        let digits   = ['0'-'9']*                        (*zero at leading and trailing integer eg. 00543, 4546000*)
        let partial  = ['1'-'9']* ['0']* ['1'-'9']       (*the non trailing partial integer eg. 4364560007*)
        let intg  = ( '0' | ['1'-'9'] ['0'-'9']*)        (*the integer regular expression helpful in another token*)
        let is = [' ']*                        (*for ignoring the space betwwen tokens*)

        (*now define the rules as*)
        (*now each token will return the type of tokens we have defined above*)
        rule tokens = parse
                 | '[' is '[' is intg is ','  is intg is ']' is ':' is '[' is intg is ',' is intg is ']' is ']' 
                                                      as c                      {RANGES c}                      (*the ranges*)
                 | '[' is  intg is ',' is intg is ']' as c                      {INDICES c}                     (*the indices*)
                 | '('                                                          {OPEN_PARENTHESIS}              (*the op mapping with character*)
                 | ')'                                                          {CLOSED_PARENTHESIS}            (*the cp mapping with character*)
                 | '['                                                          {OPEN_BRACKET}                  (*the oB mapping with character*)
                 | ']'                                                          {CLOSED_BRACKET}                (*the cB mapping with character*)
                 | ','                                                          {COMMA}                         (*just mapping*)
                 | ':'                                                          {COLON}                         (*just mapping*)
                 | [' ' '\t']                                                   {tokens lexbuf}                 (*incase of tab and whit space just ignore*)
                 | ('+'|""|'-') ('0' | nzdigits digits) '.' partial* as n       {FLOAT (n)}                     (*return float charecter*)
                 | 'R' 'O' 'W' 'M' 'A' 'X'                                      {ROWMAX}                        (*THE row max*)
                 | 'C' 'O' 'L' 'M' 'A' 'X'                                      {COLMAX}                        (*THE COL  max*) 
                 | 'M' 'A' 'X'                                                  {MAX}                           (*THE max*)
                 | 'R' 'O' 'W' 'M' 'I' 'N'                                      {ROWMIN}                        (*THE row MIN*) 
                 | 'C' 'O' 'L' 'M' 'I' 'N'                                      {COLMIN}                        (*THE COL MIN*) 
                 | 'M' 'I' 'N'                                                  {MIN}                           (*THE MIN*) 
                 | 'R' 'O' 'W' 'S' 'U' 'M'                                      {ROWSUM}                        (*THE row SUM*) 
                 | 'C' 'O' 'L' 'S' 'U' 'M'                                      {COLSUM}                        (*THE COL SUM*)
                 | 'S' 'U' 'M'                                                  {SUM}                           (*THE SUM*)
                 | 'R' 'O' 'W' 'A' 'V' 'G'                                      {ROWAVG}                        (*THE row AVG*)
                 | 'C' 'O' 'L' 'A' 'V' 'G'                                      {COLAVG}                        (*THE COL AVG*) 
                 | 'A' 'V' 'G'                                                  {AVG}                           (*THE AVG*) 
                 | 'R' 'O' 'W' 'C' 'O' 'U' 'T'                                  {ROWCOUNT}                      (*THE row COUNT*)
                 | 'C' 'O' 'L' 'C' 'O' 'U' 'T'                                  {COLCOUNT}                      (*THE COL COUNT*) 
                 | 'C' 'O' 'U' 'N' 'T'                                          {COUNT}                         (*THE COUNT*) 
                 | 'A' 'D' 'D'                                                  {ADD}                           (*THE ADD*)
                 | 'D' 'I' 'V'                                                  {DIV}                           (*THE DIV*)
                 | 'M' 'U' 'L' 'T'                                              {MULT}                          (*THE MULT*)
                 | 'S' 'U' 'B' 'T'                                              {SUBT}                          (*THE SUBT*)
                 | ['\n']                                                       {tokens lexbuf}                 (*ignore the newline*)
                 | eof  {raise Eof}                                                                             (*at end of file raise exception*)
{
        open Printf
        let main () = 
                let outstream = open_out (Sys.argv.(2)) in 
                try
                        let instream = open_in (Sys.argv.(1)) in
                        let lexbuf = Lexing.from_channel instream in
                        while true do
                                let result = tokens lexbuf in
                                match result with
                                | CLOSED_PARENTHESIS ->fprintf outstream "%s\n" "CLOSED_PARENTHESIS"; 
                                                       print_string ("CLOSED_PARENTHESIS\n");
                                | OPEN_PARENTHESIS -> fprintf outstream "%s\n" "OPEN_PARENTHESIS";
                                                      print_string ("OPEN_PARENTHESIS\n"); 
                                | CLOSED_BRACKET -> fprintf outstream "%s\n" "OPEN_BRACKET";
                                                    print_string ("CLOSED_BRACKET\n");
                                | OPEN_BRACKET -> fprintf outstream "%s\n" "OPEN_BRACKET";
                                                  print_string ("OPEN_BRACKET\n");
                                | COMMA -> fprintf outstream "%s\n" "COMMA";
                                           print_string ("COMMA\n");
                                | COLON ->  fprintf outstream "%s\n" "COLON";   
                                            print_string ("COLON\n");
                                | FLOAT n ->  fprintf outstream "%s\n" ("FLOAT: "^n);
                                              print_string ("FLOAT: "^n^"\n");
                                | RANGES n ->  fprintf outstream "%s\n" ("RANGE: "^n);
                                               print_string ("RANGE: "^n^"\n");
                                | INDICES n ->  fprintf outstream "%s\n" ("INDICE: "^n);
                                                print_string ("INDICE: "^n^"\n");
                                | COUNT ->  fprintf outstream "%s\n" "COUNT";
                                            print_string ("COUNT\n");
                                | ROWCOUNT  ->  fprintf outstream "%s\n" ("ROWCOUNT");
                                                print_string ("ROWCOUNT\n");
                                | COLCOUNT  ->  fprintf outstream "%s\n" "COLCOUNT";
                                                print_string ("COLCOUNT\n");
                                | MAX  ->  fprintf outstream "%s\n" "MAX";
                                           print_string ("MAX\n");
                                | ROWMAX  ->  fprintf outstream "%s\n" "ROWMAX";
                                              print_string ("ROWMAX\n");
                                | COLMAX  ->  fprintf outstream "%s\n" "COLMAX";
                                              print_string ("COLMAX\n");
                                | SUM  ->  fprintf outstream "%s\n" "SUM";
                                           print_string ("SUM\n");
                                | ROWSUM  ->  fprintf outstream "%s\n" "ROWSUM";
                                              print_string ("ROWSUM\n");
                                | COLSUM   ->  fprintf outstream "%s\n" "COLSUM";
                                               print_string ("COLSUM\n");
                                | MIN   ->  fprintf outstream "%s\n" "MIN";
                                            print_string ("MIN\n");
                                | ROWMIN  ->  fprintf outstream "%s\n" "ROWMIN";
                                              print_string ("ROWMIN\n");
                                | COLMIN  ->  fprintf outstream "%s\n" "COLMIN";
                                              print_string ("COLMIN\n");
                                | AVG  ->  fprintf outstream "%s\n" "AVG";
                                           print_string ("AVG\n");
                                | ROWAVG  -> fprintf outstream "%s\n" "ROWAVG";
                                             print_string ("ROWAVG\n");
                                | COLAVG  ->  fprintf outstream "%s\n" "COLAVG";
                                              print_string ("COLAVG\n");
                                | ADD  ->  fprintf outstream "%s\n" "ADD";
                                           print_string ("ADD\n");
                                | SUBT  ->  fprintf outstream "%s\n" "SUBT";
                                            print_string ("SUBT\n");
                                | MULT  ->  fprintf outstream "%s\n" "MULT";
                                            print_string ("MULT\n");
                                | DIV  ->  fprintf outstream "%s\n" "DIV";
                                           print_string ("DIV\n");
                        done;
                with Eof -> close_out outstream; exit 0;;

        main ();;
}
