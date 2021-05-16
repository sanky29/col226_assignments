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

open Parsing;;
let _ = parse_error;;
# 2 "Parser.mly"

        (*we have decided to make the parse tree and give this as input
         * to the main which will also take eval input and perform the
         * given requirments like 
         *                      1. type checking
         *                      2. evaluation                           *)
        open Lexing
        open Printf
        open Tree
# 46 "Parser.ml"
let yytransl_const = [|
  257 (* NEWLINE *);
  258 (* CLOSED_PARENTHESIS *);
  259 (* OPEN_PARENTHESIS *);
  260 (* OPEN_BRACKET *);
  261 (* CLOSE_BRACKET *);
  262 (* ASSIGN *);
  263 (* COMMA *);
  264 (* SEMICOL *);
  265 (* COUNT *);
  266 (* ROWCOUNT *);
  267 (* COLCOUNT *);
  268 (* MAX *);
  269 (* ROWMAX *);
  270 (* COLMAX *);
  271 (* MIN *);
  272 (* ROWMIN *);
  273 (* COLMIN *);
  274 (* SUM *);
  275 (* ROWSUM *);
  276 (* COLSUM *);
  277 (* AVG *);
  278 (* ROWAVG *);
  279 (* COLAVG *);
  280 (* ADD *);
  281 (* MULT *);
  282 (* SUBT *);
  283 (* DIV *);
    0|]

let yytransl_block = [|
  284 (* NUM *);
  285 (* INDICES *);
  286 (* RANGES *);
    0|]

let yylhs = "\255\255\
\002\000\002\000\003\000\003\000\004\000\004\000\005\000\005\000\
\001\000\006\000\006\000\007\000\007\000\007\000\007\000\007\000\
\007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
\007\000\007\000\007\000\008\000\008\000\008\000\008\000\008\000\
\008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
\008\000\008\000\008\000\008\000\008\000\008\000\008\000\000\000"

let yylen = "\002\000\
\001\000\003\000\001\000\003\000\001\000\003\000\001\000\003\000\
\002\000\004\000\004\000\003\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\005\000\048\000\000\000\000\000\007\000\
\000\000\000\000\000\000\000\000\009\000\000\000\006\000\008\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\003\000\019\000\021\000\020\000\013\000\014\000\015\000\016\000\
\017\000\018\000\025\000\026\000\027\000\022\000\023\000\024\000\
\000\000\001\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\010\000\011\000\
\012\000\000\000\000\000\036\000\035\000\037\000\034\000\033\000\
\041\000\040\000\042\000\039\000\038\000\031\000\030\000\032\000\
\029\000\028\000\046\000\045\000\047\000\044\000\043\000\004\000\
\002\000"

let yydgoto = "\002\000\
\005\000\059\000\042\000\006\000\007\000\008\000\037\000\038\000"

let yysindex = "\020\000\
\031\255\000\000\034\255\000\000\000\000\016\255\022\255\000\000\
\251\254\032\255\035\255\073\255\000\000\032\255\000\000\000\000\
\099\255\027\255\027\255\027\255\027\255\027\255\027\255\027\255\
\027\255\027\255\027\255\027\255\027\255\027\255\027\255\027\255\
\051\255\051\255\051\255\051\255\026\255\029\255\036\255\024\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\246\254\000\000\027\255\051\255\027\255\027\255\051\255\027\255\
\027\255\051\255\027\255\027\255\051\255\027\255\000\000\000\000\
\000\000\061\255\063\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\062\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000"

let yygindex = "\000\000\
\000\000\252\255\237\255\248\255\000\000\064\000\052\000\000\000"

let yytablesize = 122
let yytable = "\043\000\
\044\000\045\000\046\000\047\000\048\000\049\000\050\000\051\000\
\052\000\053\000\054\000\055\000\056\000\060\000\063\000\066\000\
\069\000\075\000\014\000\074\000\001\000\012\000\013\000\014\000\
\061\000\064\000\067\000\070\000\040\000\062\000\065\000\068\000\
\003\000\071\000\015\000\009\000\072\000\016\000\073\000\076\000\
\078\000\080\000\081\000\083\000\085\000\086\000\088\000\090\000\
\091\000\093\000\095\000\079\000\057\000\074\000\084\000\077\000\
\041\000\089\000\082\000\004\000\094\000\087\000\010\000\096\000\
\092\000\097\000\011\000\005\000\039\000\000\000\000\000\000\000\
\000\000\000\000\017\000\000\000\000\000\000\000\058\000\004\000\
\041\000\018\000\019\000\020\000\021\000\022\000\023\000\024\000\
\025\000\026\000\027\000\028\000\029\000\030\000\031\000\032\000\
\033\000\034\000\035\000\036\000\017\000\000\000\000\000\000\000\
\000\000\000\000\000\000\018\000\019\000\020\000\021\000\022\000\
\023\000\024\000\025\000\026\000\027\000\028\000\029\000\030\000\
\031\000\032\000"

let yycheck = "\019\000\
\020\000\021\000\022\000\023\000\024\000\025\000\026\000\027\000\
\028\000\029\000\030\000\031\000\032\000\033\000\034\000\035\000\
\036\000\028\001\029\001\030\001\001\000\006\001\001\001\029\001\
\033\000\034\000\035\000\036\000\002\001\034\000\035\000\036\000\
\002\001\008\001\003\001\002\001\008\001\003\001\003\001\059\000\
\060\000\061\000\062\000\063\000\064\000\065\000\066\000\067\000\
\068\000\069\000\070\000\060\000\002\001\030\001\063\000\060\000\
\030\001\066\000\063\000\029\001\069\000\066\000\029\001\003\001\
\069\000\003\001\003\000\006\001\017\000\255\255\255\255\255\255\
\255\255\255\255\002\001\255\255\255\255\255\255\028\001\029\001\
\030\001\009\001\010\001\011\001\012\001\013\001\014\001\015\001\
\016\001\017\001\018\001\019\001\020\001\021\001\022\001\023\001\
\024\001\025\001\026\001\027\001\002\001\255\255\255\255\255\255\
\255\255\255\255\255\255\009\001\010\001\011\001\012\001\013\001\
\014\001\015\001\016\001\017\001\018\001\019\001\020\001\021\001\
\022\001\023\001"

let yynames_const = "\
  NEWLINE\000\
  CLOSED_PARENTHESIS\000\
  OPEN_PARENTHESIS\000\
  OPEN_BRACKET\000\
  CLOSE_BRACKET\000\
  ASSIGN\000\
  COMMA\000\
  SEMICOL\000\
  COUNT\000\
  ROWCOUNT\000\
  COLCOUNT\000\
  MAX\000\
  ROWMAX\000\
  COLMAX\000\
  MIN\000\
  ROWMIN\000\
  COLMIN\000\
  SUM\000\
  ROWSUM\000\
  COLSUM\000\
  AVG\000\
  ROWAVG\000\
  COLAVG\000\
  ADD\000\
  MULT\000\
  SUBT\000\
  DIV\000\
  "

let yynames_block = "\
  NUM\000\
  INDICES\000\
  RANGES\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 31 "Parser.mly"
                   (Zeroary (CONST (_1)))
# 230 "Parser.ml"
               : 'const))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : float) in
    Obj.repr(
# 31 "Parser.mly"
                                                                                     (Zeroary (CONST (_2)))
# 237 "Parser.ml"
               : 'const))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int array) in
    Obj.repr(
# 32 "Parser.mly"
                      (Zeroary (RANGE (_1:range)))
# 244 "Parser.ml"
               : 'range))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : int array) in
    Obj.repr(
# 32 "Parser.mly"
                                                                                                 (Zeroary (RANGE (_2:range)))
# 251 "Parser.ml"
               : 'range))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int array) in
    Obj.repr(
# 33 "Parser.mly"
                         (Zeroary (INDICE (_1:index)))
# 258 "Parser.ml"
               : 'indice))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : int array) in
    Obj.repr(
# 33 "Parser.mly"
                                                                                                     (Zeroary (INDICE (_2:index)))
# 265 "Parser.ml"
               : 'indice))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 34 "Parser.mly"
                   (_1)
# 272 "Parser.ml"
               : 'main2))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'exp) in
    Obj.repr(
# 34 "Parser.mly"
                                                                  (_2)
# 279 "Parser.ml"
               : 'main2))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'main2) in
    Obj.repr(
# 35 "Parser.mly"
                            (_1)
# 286 "Parser.ml"
               : Tree.node*Tree.node))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'indice) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'ufunct) in
    Obj.repr(
# 36 "Parser.mly"
                                          (_1,_3)
# 294 "Parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'indice) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'bfunct) in
    Obj.repr(
# 36 "Parser.mly"
                                                                                 (_1,_3)
# 302 "Parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'ufunct) in
    Obj.repr(
# 38 "Parser.mly"
                                                             (_2)
# 309 "Parser.ml"
               : 'ufunct))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'range) in
    Obj.repr(
# 39 "Parser.mly"
                             (Unary (MAX,_2))
# 316 "Parser.ml"
               : 'ufunct))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'range) in
    Obj.repr(
# 40 "Parser.mly"
                                (Unary (ROWMAX,_2))
# 323 "Parser.ml"
               : 'ufunct))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'range) in
    Obj.repr(
# 41 "Parser.mly"
                                (Unary (COLMAX,_2))
# 330 "Parser.ml"
               : 'ufunct))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'range) in
    Obj.repr(
# 42 "Parser.mly"
                             (Unary (MIN,_2))
# 337 "Parser.ml"
               : 'ufunct))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'range) in
    Obj.repr(
# 43 "Parser.mly"
                               (Unary (ROWMIN,_2))
# 344 "Parser.ml"
               : 'ufunct))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'range) in
    Obj.repr(
# 44 "Parser.mly"
                                (Unary (COLMIN,_2))
# 351 "Parser.ml"
               : 'ufunct))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'range) in
    Obj.repr(
# 45 "Parser.mly"
                               (Unary (COUNT,_2))
# 358 "Parser.ml"
               : 'ufunct))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'range) in
    Obj.repr(
# 46 "Parser.mly"
                                  (Unary (COLCOUNT,_2))
# 365 "Parser.ml"
               : 'ufunct))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'range) in
    Obj.repr(
# 47 "Parser.mly"
                                  (Unary (ROWCOUNT,_2))
# 372 "Parser.ml"
               : 'ufunct))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'range) in
    Obj.repr(
# 48 "Parser.mly"
                             (Unary (AVG,_2))
# 379 "Parser.ml"
               : 'ufunct))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'range) in
    Obj.repr(
# 49 "Parser.mly"
                               (Unary (ROWAVG,_2))
# 386 "Parser.ml"
               : 'ufunct))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'range) in
    Obj.repr(
# 50 "Parser.mly"
                               (Unary (COLAVG,_2))
# 393 "Parser.ml"
               : 'ufunct))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'range) in
    Obj.repr(
# 51 "Parser.mly"
                             (Unary (SUM,_2))
# 400 "Parser.ml"
               : 'ufunct))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'range) in
    Obj.repr(
# 52 "Parser.mly"
                               (Unary (ROWSUM,_2))
# 407 "Parser.ml"
               : 'ufunct))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'range) in
    Obj.repr(
# 53 "Parser.mly"
                               (Unary (COLSUM,_2))
# 414 "Parser.ml"
               : 'ufunct))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'indice) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'range) in
    Obj.repr(
# 56 "Parser.mly"
                                    (Binary (SUBT,_2,_3))
# 422 "Parser.ml"
               : 'bfunct))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'range) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'indice) in
    Obj.repr(
# 57 "Parser.mly"
                                    (Binary (SUBT,_2,_3))
# 430 "Parser.ml"
               : 'bfunct))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'range) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'const) in
    Obj.repr(
# 58 "Parser.mly"
                                    (Binary (SUBT,_2,_3))
# 438 "Parser.ml"
               : 'bfunct))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'const) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'range) in
    Obj.repr(
# 59 "Parser.mly"
                                    (Binary (SUBT,_2,_3))
# 446 "Parser.ml"
               : 'bfunct))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'range) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'range) in
    Obj.repr(
# 60 "Parser.mly"
                                    (Binary (SUBT,_2,_3))
# 454 "Parser.ml"
               : 'bfunct))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'indice) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'range) in
    Obj.repr(
# 61 "Parser.mly"
                                    (Binary (ADD,_2,_3))
# 462 "Parser.ml"
               : 'bfunct))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'range) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'indice) in
    Obj.repr(
# 62 "Parser.mly"
                                   (Binary (ADD,_2,_3))
# 470 "Parser.ml"
               : 'bfunct))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'range) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'const) in
    Obj.repr(
# 63 "Parser.mly"
                                   (Binary (ADD,_2,_3))
# 478 "Parser.ml"
               : 'bfunct))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'const) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'range) in
    Obj.repr(
# 64 "Parser.mly"
                                   (Binary (ADD,_2,_3))
# 486 "Parser.ml"
               : 'bfunct))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'range) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'range) in
    Obj.repr(
# 65 "Parser.mly"
                                   (Binary (ADD,_2,_3))
# 494 "Parser.ml"
               : 'bfunct))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'indice) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'range) in
    Obj.repr(
# 66 "Parser.mly"
                                    (Binary (MULT,_2,_3))
# 502 "Parser.ml"
               : 'bfunct))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'range) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'indice) in
    Obj.repr(
# 67 "Parser.mly"
                                    (Binary (MULT,_2,_3))
# 510 "Parser.ml"
               : 'bfunct))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'range) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'const) in
    Obj.repr(
# 68 "Parser.mly"
                                    (Binary (MULT,_2,_3))
# 518 "Parser.ml"
               : 'bfunct))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'const) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'range) in
    Obj.repr(
# 69 "Parser.mly"
                                    (Binary (MULT,_2,_3))
# 526 "Parser.ml"
               : 'bfunct))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'range) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'range) in
    Obj.repr(
# 70 "Parser.mly"
                                    (Binary (MULT,_2,_3))
# 534 "Parser.ml"
               : 'bfunct))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'indice) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'range) in
    Obj.repr(
# 71 "Parser.mly"
                                   (Binary (DIV,_2,_3))
# 542 "Parser.ml"
               : 'bfunct))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'range) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'indice) in
    Obj.repr(
# 72 "Parser.mly"
                                   (Binary (DIV,_2,_3))
# 550 "Parser.ml"
               : 'bfunct))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'range) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'const) in
    Obj.repr(
# 73 "Parser.mly"
                                   (Binary (DIV,_2,_3))
# 558 "Parser.ml"
               : 'bfunct))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'const) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'range) in
    Obj.repr(
# 74 "Parser.mly"
                                   (Binary (DIV,_2,_3))
# 566 "Parser.ml"
               : 'bfunct))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'range) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'range) in
    Obj.repr(
# 75 "Parser.mly"
                                   (Binary (DIV,_2,_3))
# 574 "Parser.ml"
               : 'bfunct))
(* Entry main *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let main (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Tree.node*Tree.node)
