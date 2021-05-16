
let main () =

       try
              while true do 
               let lexbuf = Lexing.from_channel stdin in
               let x =   T.main La.token lexbuf in
               print_string ("i am done\n");
               print_string(string_of_int (T2.depth x)^"\n");
              done;
       with End_of_file -> exit 0;;
let _ = main ()
