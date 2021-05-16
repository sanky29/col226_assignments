ocamlyacc T.mly
ocamlc -c T.mli
ocamlc -c T.ml
ocamllex La.mll
ocamlc -c La.ml
ocamlc -c  main.ml
ocamlc -o calc T2.cmo La.cmo T.cmo main.cmo
./calc
