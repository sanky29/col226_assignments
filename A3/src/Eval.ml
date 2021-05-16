open Tree;;
open String;;
open Printf;;
open List;;
open Tree;;
open Array;;
let read_file s =
                let infile = open_in s in
                let ans = ref [] in
              try  while true do
                      begin
                        let l = input_line infile in
                        ans := l::(!ans);
                      end;
                done;
                !ans;
              with  End_of_file -> close_in infile;
             List.rev !ans;;

let string_to_cell c = 
        match c with
        | "" -> Null
        | h -> Value (float_of_string h);;

let list_of_csv = read_file Sys.argv.(1);;                                         
let trimed_list_of_csv = List.map trim list_of_csv;;
let splitted_list_of_csv = List.map (split_on_char ',') trimed_list_of_csv;;
let sheet_of_csv =List.map (List.map string_to_cell) splitted_list_of_csv
let sheet:sheet = of_list (List.map of_list sheet_of_csv);;
print_string "---THE INPUT FILE---\n";;
print sheet;;
print_string "--------------------\n";;

let get1 x =
	match x with
	| _,a-> a;;

let cell_to_string c=
      match c with
      | Null -> "";
      | Value t -> string_of_float t;;
        
exception WrongType of string;;
let c = ref sheet;;
let main ans =
        let infile = open_in Sys.argv.(4) in
        try 
                while true do
                        let lexbuf = Lexing.from_channel infile in
                        let x = Parser.main Lexer.tokens lexbuf in
                        Tree.type_check (get1 x);
         				ans := Tree.execute ans x;
     					done;
        with End_of_file ->
        close_in infile; 
        print_string "---THE OUTPUT FILE---\n";
        Tree.print !ans;
        print_string "---------------\n";
        let write_to_file c =
            for i = 0 to Array.length !ans - 1 do
                  for j = 0 to Array.length !ans.(0) - 1 do
                      fprintf c "%s" ((cell_to_string !ans.(i).(j))^",");
                  done;
              fprintf c "%s\n" "";
            done;
            close_out c;
        in 
        let s = (hd (split_on_char '.' Sys.argv.(1)))^"_output.csv" in
        write_to_file (open_out s);
        exit 0;;

let _ = main c;;
