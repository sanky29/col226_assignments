ocamlc eval.ml
ocamlyacc Parser.mly
ocamlc -c Parser.mli
ocamlc -c Parser.ml
ocamllex Lexer.mll
ocamlc -c Lexer.ml
ocamlc eval.cmo Parser.cmo Lexer.cmo -o a.exe main.ml
./a.exe
./refresh.sh
