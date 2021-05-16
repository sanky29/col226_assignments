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

open Parsing;;
let _ = parse_error;;
# 1 "Parser.mly"
      
         open Printf;;
         open Eval;;             (*the first code block*)
         open String;;           (*the tokenizer for our file*)
         (*some helpful function*)
         exception Eof;;
# 22 "Parser.ml"
let yytransl_const = [|
  260 (* COMMA *);
  261 (* ENDLINE *);
  262 (* ASSIGN *);
  263 (* CP *);
    0 (* EOF *);
  264 (* OP *);
    0|]

let yytransl_block = [|
  257 (* UPPER_STRING *);
  258 (* LOWER_STRING *);
  259 (* FUNAME *);
    0|]

let yylhs = "\255\255\
\003\000\003\000\003\000\004\000\004\000\004\000\004\000\005\000\
\005\000\005\000\006\000\006\000\007\000\001\000\001\000\001\000\
\002\000\002\000\000\000\000\000"

let yylen = "\002\000\
\001\000\001\000\003\000\001\000\001\000\003\000\003\000\002\000\
\003\000\003\000\001\000\003\000\003\000\003\000\003\000\001\000\
\002\000\001\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\016\000\000\000\019\000\000\000\
\000\000\018\000\020\000\000\000\000\000\001\000\002\000\008\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\017\000\000\000\000\000\010\000\000\000\009\000\015\000\
\013\000\014\000\012\000\003\000\006\000\007\000"

let yydgoto = "\003\000\
\007\000\011\000\018\000\019\000\008\000\013\000\009\000"

let yysindex = "\033\000\
\001\000\002\000\000\000\009\255\000\000\000\255\000\000\031\255\
\251\254\000\000\000\000\010\255\013\255\000\000\000\000\000\000\
\021\255\023\255\018\255\026\255\025\255\001\000\000\255\001\000\
\000\255\000\000\032\255\021\255\000\000\021\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\033\255\000\000\000\000\000\000\000\000\
\000\000\034\255\000\000\035\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\238\255\000\000\023\000\241\255\003\000\252\255\000\000"

let yytablesize = 266
let yytable = "\024\000\
\005\000\010\000\004\000\032\000\012\000\034\000\020\000\006\000\
\021\000\014\000\015\000\004\000\037\000\025\000\038\000\016\000\
\017\000\026\000\033\000\021\000\035\000\014\000\015\000\004\000\
\029\000\012\000\028\000\012\000\017\000\030\000\020\000\031\000\
\020\000\001\000\002\000\022\000\023\000\011\000\036\000\027\000\
\004\000\005\000\000\000\000\000\000\000\000\000\000\000\000\000\
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
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\004\000\004\000\000\000\000\000\000\000\
\006\000\006\000"

let yycheck = "\005\001\
\000\000\000\000\003\001\022\000\002\000\024\000\004\000\008\001\
\006\000\001\001\002\001\003\001\028\000\004\001\030\000\007\001\
\008\001\005\001\023\000\017\000\025\000\001\001\002\001\003\001\
\007\001\023\000\004\001\025\000\008\001\004\001\028\000\007\001\
\030\000\001\000\002\000\005\001\006\001\005\001\007\001\017\000\
\007\001\007\001\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\003\001\003\001\255\255\255\255\255\255\
\008\001\008\001"

let yynames_const = "\
  COMMA\000\
  ENDLINE\000\
  ASSIGN\000\
  CP\000\
  EOF\000\
  OP\000\
  "

let yynames_block = "\
  UPPER_STRING\000\
  LOWER_STRING\000\
  FUNAME\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 27 "Parser.mly"
                             (Var (_1:variable))
# 170 "Parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 28 "Parser.mly"
                             (Const (_1:constant))
# 177 "Parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 29 "Parser.mly"
                           (_2)
# 184 "Parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 32 "Parser.mly"
                     ([_1])
# 191 "Parser.ml"
               : 'term_seq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'af) in
    Obj.repr(
# 33 "Parser.mly"
                   ([Funct _1])
# 198 "Parser.ml"
               : 'term_seq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term_seq) in
    Obj.repr(
# 34 "Parser.mly"
                                    (_1::_3)
# 206 "Parser.ml"
               : 'term_seq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'af) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term_seq) in
    Obj.repr(
# 35 "Parser.mly"
                                  ((Funct _1)::_3)
# 214 "Parser.ml"
               : 'term_seq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 38 "Parser.mly"
                           (((Eval.f _1),0),[])
# 221 "Parser.ml"
               : 'af))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'af) in
    Obj.repr(
# 39 "Parser.mly"
                         (_2)
# 228 "Parser.ml"
               : 'af))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'term_seq) in
    Obj.repr(
# 40 "Parser.mly"
                                   (((f _1) , List.length _2),_2)
# 236 "Parser.ml"
               : 'af))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'af) in
    Obj.repr(
# 43 "Parser.mly"
                   ([_1])
# 243 "Parser.ml"
               : 'af_seq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'af) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'af_seq) in
    Obj.repr(
# 44 "Parser.mly"
                                (_1::_3)
# 251 "Parser.ml"
               : 'af_seq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'af) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'af_seq) in
    Obj.repr(
# 46 "Parser.mly"
                                 (Rule (_1,_3))
# 259 "Parser.ml"
               : 'rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Eval.clause list) in
    Obj.repr(
# 49 "Parser.mly"
                                       (_1::_3)
# 267 "Parser.ml"
               : Eval.clause list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'af) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Eval.clause list) in
    Obj.repr(
# 50 "Parser.mly"
                                     ((Fact _1)::_3)
# 275 "Parser.ml"
               : Eval.clause list))
; (fun __caml_parser_env ->
    Obj.repr(
# 51 "Parser.mly"
                    ([])
# 281 "Parser.ml"
               : Eval.clause list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'af_seq) in
    Obj.repr(
# 54 "Parser.mly"
                           (_1)
# 288 "Parser.ml"
               : Eval.af list))
; (fun __caml_parser_env ->
    Obj.repr(
# 55 "Parser.mly"
                  ([])
# 294 "Parser.ml"
               : Eval.af list))
(* Entry main_file *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry main_shell *)
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
let main_file (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Eval.clause list)
let main_shell (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 2 lexfun lexbuf : Eval.af list)
;;
# 60 "Parser.mly"

(*the last block of code
  af   ENDLINE main {Fact $1}
              | rule ENDLINE main {$1}



              *)



# 335 "Parser.ml"
