type token =
  | NEWLINE
  | CLOSED_PARENTHESIS
  | OPEN_PARENTHESIS
  | OPEN_BRACKET
  | CLOSE_BRACKET
  | ASSIGN
  | COMMA
  | SEMICOL
  | COUNT
  | ROWCOUNT
  | COLCOUNT
  | MAX
  | ROWMAX
  | COLMAX
  | MIN
  | ROWMIN
  | COLMIN
  | SUM
  | ROWSUM
  | COLSUM
  | AVG
  | ROWAVG
  | COLAVG
  | ADD
  | MULT
  | SUBT
  | DIV
  | NUM of (float)
  | INDICES of (int array)
  | RANGES of (int array)

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Tree.node*Tree.node
