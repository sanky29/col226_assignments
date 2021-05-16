(*the header seaction 
the lexifier starts working from here*)
{                
                open Parser;;
                open String;;
                open List;;
                open Array;;

		(*the spliting function for the string*)
		let rec joint s =
		match s with
			| [] -> ""
			| ""::t->joint t
			| h::t -> h^(joint t);;

		(*the convert function for the string*)	
		let convert s =
			let s1 = ref s in
			let l1 = ref (split_on_char ' ' s) in 
			s1 := joint !l1;
			l1 := split_on_char '[' !s1;
			s1 := joint !l1;
			l1 := split_on_char ']' !s1;
			s1 := joint !l1;
                        l1 := split_on_char '(' !s1;
                        s1 := joint !l1;
                        l1 := split_on_char ')' !s1;
                        s1 := joint !l1;
			l1 := split_on_char ':' !s1;
			begin match !l1 with
				| x::y::z -> s1 := x^","^y;
				| _ -> (); end; 
			l1 := split_on_char ',' !s1;
			let ans = List.map int_of_string !l1 in
                        of_list ans;;

                let convert2 s =
                        let s1 = ref s in
                        let l1 = ref (split_on_char ' ' s) in
                        s1 := joint !l1;
                        l1 := split_on_char '[' !s1;
                        s1 := joint !l1;
                        l1 := split_on_char ']' !s1;
                        s1 := joint !l1;
                        l1 := split_on_char ',' !s1;
                        let ans = List.map int_of_string !l1 in
                        of_list ans;;
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
                 | '(' is '[' is intg is ','  is intg is ']' is ':' is '[' is intg is ',' is intg is ']' is ')' 
                                                      as c                      {RANGES (convert c)}            (*the ranges*)
                 | '[' is  intg is ',' is intg is ']' as c                      {INDICES (convert2 c)}          (*the indices*)
                 | '('                                                          {OPEN_PARENTHESIS}              (*the op mapping with character*)
                 | ')'                                                          {CLOSED_PARENTHESIS}            (*the cp mapping with character*)
                 | '['                                                          {OPEN_BRACKET}                  (*the oB mapping with character*)
                 | ':' '='                                                      {ASSIGN}
                 | ']'                                                          {CLOSE_BRACKET}                 (*the cB mapping with character*)
                 | ','                                                          {COMMA}                         (*just mapping*) 
                 | ';'                                                          {SEMICOL}                       (*just mapping*)
                 | [' ' '\t']                                                   {tokens lexbuf}                 (*incase of tab and whit space just ignore*)
                 | ('+'|""|'-') ('0' | nzdigits digits) '.' partial* as n       {NUM (float_of_string n)}       (*return float charecter*)
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
                 | ('\n'|'\r')                                                       {NEWLINE}                       (*ignore the newline*)
                 | eof                                                          {raise End_of_file}                           (*at end of file raise exception*)
