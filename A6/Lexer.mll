{	(*the first block of code*)
open Parser;;
exception Eof;;
}

(*define upper ans lower string*)
let lower = ['a'-'z'] ['a'-'z''A'-'Z']*
let up = ['A'-'Z']['a'-'z''A'-'Z']*

rule tokens = parse
	| '('  {OP}
	|  ')' {CP}
	| lower '(' as s {FUNAME (s)}
	| lower as s {LOWER_STRING (s)}
	| up as s {UPPER_STRING (s)}
	| ':' '-' {ASSIGN}
	| ',' {COMMA}
	| ['\n' '\r' '\t' ' '] {tokens lexbuf}
	| '.' ['\n' '\r' '\t' ' '] {ENDLINE}
	| eof {EOF}
