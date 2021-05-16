{
        open T

}
        let digit = ['0'-'9']*
        let nzdigits = ['1'-'9']
        let partial = ['1'-'9']* ['0']* ['1'-'9']

        rule token = parse
        | ')' { CLOSE_PARENTHESIS }
        | '(' { OPEN_PARENTHESIS}
        | '+' {PLUS}
        | '-' { MINUS}
        | '/' {DIV}
        | '*' { MUL}
        | '\n' {NEWLINE}
        | ( '0' | nzdigits digit ) '.' partial* as c { NUM (float_of_string c)}
        | [' ' '\t'] {token lexbuf}
        | _ {token lexbuf}
        | eof {raise End_of_file}

