type token =
  | UPPER_STRING of (string)
  | LOWER_STRING of (string)
  | FUNAME of (string)
  | COMMA
  | ENDLINE
  | ASSIGN
  | CP
  | EOF
  | OP

val main_file :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Eval.clause list
val main_shell :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Eval.af list
