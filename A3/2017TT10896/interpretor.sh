ocamlc Tree.ml
ocamlyacc Parser.mly
ocamlc -c Parser.mli
ocamlc -c Parser.ml
ocamllex Lexer.mll
ocamlc -c Lexer.ml
ocamlc -c  Eval.ml
ocamlc -o assignment4 Tree.cmo Lexer.cmo Parser.cmo Eval.cmo
