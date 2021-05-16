open Array;;
type range = 
        int array

type index =
        int array

type signature =
        | RANGE of range
        | CONST of float
        | INDICE of index
        | COUNT| ROWCOUNT | COLCOUNT 
        | SUM | ROWSUM | COLSUM  
        | AVG | ROWAVG | COLAVG 
        | MIN | ROWMIN | COLMIN
        | MAX | ROWMAX | COLMAX
        | ADD | SUBT | MULT | DIV;;

type node =
        Leaf
        | Zeroary of signature
        | Unary of signature*node
        | Binary of signature*node*node

type cell =
        | Null
        | Value of float

type sheet =
        cell array array

exception NullValue of string
exception SytaxErrorLSP
exception WrongType of string

let update (x:sheet) n1 m1:sheet =
    let m_old =Array.length x.(0) in
    let n_old =Array.length x in
    let n = max (n1+1) n_old in
    let m = max (m1+1) m_old in
    let f i =
        let temp = Array.make m Null in 
        begin if i < n_old then
            Array.blit x.(i) 0 temp 0 m_old end;
        temp in
    (Array.init n f);;

let type_check n =
    match n with
    | Binary (a,b,c) -> begin 
                        match b,c with
                            |  Zeroary f, Zeroary e -> 
                                begin
                                    match f,e with
                                        | RANGE r1, RANGE r2 -> 
                                        begin
                                        if( r1.(2) < r1.(0) || r2.(2) < r2.(0) || r1.(3) < r1.(1) || r2.(3) < r2.(1)
                                          || r1.(2)-r1.(0) != r2.(2)-r2.(0) || r1.(3)-r1.(1) !=  r2.(3)-r2.(1)) then
                                        raise (WrongType ("the indices should be in acending order ans two ranges should be of equal size\n"));
                                    end
                                        | RANGE r1, _ -> 
                                        begin
                                             if (r1.(2) < r1.(0) || r1.(3) < r1.(1)) 
                                             then
                                              raise (WrongType ("the indices should be in acending order ans two ranges should be of equal size "
                                            ^(string_of_int r1.(0))^" "^(string_of_int r1.(1))^"\n"));
                                 
                                         end 
                                        | _ , RANGE r1->  
                                         begin
                                             if (r1.(2) < r1.(0) || r1.(3) < r1.(1)) 
                                             then raise (WrongType ("the indices should be in acending order ans two ranges should be of equal size "
                                            ^(string_of_int r1.(0))^" "^(string_of_int r1.(1))^"\n"));
                                 
                                         end 
                                        | _ -> ();
                                end;

                            | _ -> ();
                    end
    | Unary (a,b) ->
                    begin
                        match  b with
                            | Zeroary (RANGE r1)-> 
                            begin
                                             if (r1.(2) < r1.(0) || r1.(3) < r1.(1)) 
                                             then raise (WrongType ("the indices should be in acending order ans two ranges should be of equal size "
                                            ^(string_of_int r1.(0))^" "^(string_of_int r1.(1))^"\n"));
                                         end 
                            | _ -> ();
                    end
    | _ -> ();;

(*no we will write helper funvtions for the evaluation*)
let full_count (x:sheet) (y:range) (z:index)=
        let ans = ref 0.0 in
        for i = y.(0) to y.(2) do
                for j = y.(1) to y.(3) do
                         match x.(i).(j) with
                                 | Null -> ();
                                 | _ -> ans := !ans +. 1.0;
               done;
        done;
        x.(z.(0)).(z.(1)) <-Value (!ans);
        x;;

let row_count (x:sheet) (y:range) (z:index)=
      for i=y.(0) to y.(2) do
                let temp = ref 0.0 in
                for j=y.(1) to y.(3) do
                    match x.(i).(j) with
                        | Null -> ();
                        | _ -> temp := !temp +. 1.0;
                done;
                x.(z.(0)+i-y.(0)).(z.(1)) <- Value !temp;                                                                                                                   done;
    x;;

let col_count (x:sheet) (y:range) (z:index)=
      for j=y.(1) to y.(3) do
                let temp = ref 0.0 in
                for i=y.(0) to y.(2) do
                    match x.(i).(j) with
                        | Null -> ();
                        | _ -> temp := !temp +. 1.0;
                done;
                x.(z.(0)).(z.(1)+j-y.(1)) <- Value !temp;                                                                                                                   done;
    x;;

let full_sum (x:sheet) (y:range) (z:index)=
        let ans = ref 0.0 in
        for i = y.(0) to y.(2) do
                for j = y.(1) to y.(3) do
                         match x.(i).(j) with
                                 | Null -> raise (NullValue ("Null value at "^(string_of_int i)^","^(string_of_int j)));
                                 | Value t -> ans := !ans +. t;
               done;
        done;
        x.(z.(0)).(z.(1)) <-Value (!ans);
        x;;

let row_sum (x:sheet) (y:range) (z:index)=
        for i = y.(0) to y.(2) do
                let ans = ref 0.0 in
                for j = y.(1) to y.(3) do
                         match x.(i).(j) with
                                 | Null -> raise (NullValue ("Null value at "^(string_of_int i)^","^(string_of_int j)));
                                 | Value t -> ans := !ans +. t;
               done;
               x.(z.(0)+i-y.(0)).(z.(1)) <- Value !ans;  
        done;
        x;;

let col_sum (x:sheet) (y:range) (z:index)=
        for j = y.(1) to y.(3) do
                let ans = ref 0.0 in
                for i = y.(0) to y.(2) do
                         match x.(i).(j) with
                                 | Null -> raise (NullValue ("Null value at "^(string_of_int i)^","^(string_of_int j)));
                                 | Value t -> ans := !ans +. t;
               done;
               x.(z.(0)).(z.(1)+j-y.(1)) <- Value !ans;  
        done;
        x;;

let full_avg (x:sheet) (y:range) (z:index)=
        let ans = ref 0.0 in
        for i = y.(0) to y.(2) do
                for j = y.(1) to y.(3) do
                         match x.(i).(j) with
                                 | Null -> raise (NullValue ("Null value at "^(string_of_int i)^","^(string_of_int j)));
                                 | Value t -> ans := !ans +. t;
               done;
        done;
        let n = float_of_int ((y.(2) - y.(0))*(y.(3) - y.(1))) in
        x.(z.(0)).(z.(1)) <-Value (!ans /. n);
        x;;

let row_avg (x:sheet) (y:range) (z:index)=
        for i = y.(0) to y.(2) do
                let ans = ref 0.0 in
                for j = y.(1) to y.(3) do
                         match x.(i).(j) with
                                 | Null -> raise (NullValue ("Null value at "^(string_of_int i)^","^(string_of_int j)));
                                 | Value t -> ans := !ans +. t;
               done;
               x.(z.(0)+i-y.(0)).(z.(1)) <- Value (!ans/.(float_of_int (y.(3) - y.(1))));  
        done;
        x;;

let col_avg (x:sheet) (y:range) (z:index)=
        for j = y.(1) to y.(3) do
                let ans = ref 0.0 in
                for i = y.(0) to y.(2) do
                         match x.(i).(j) with
                                 | Null -> raise (NullValue ("Null value at "^(string_of_int i)^","^(string_of_int j)));
                                 | Value t -> ans := !ans +. t;
               done;
               x.(z.(0)).(z.(1)+j-y.(1)) <- Value (!ans/.(float_of_int (y.(2) - y.(0))));  
        done;
        x;;

let full_max (x:sheet) (y:range) (z:index)=
        let ans = ref min_float in
        for i = y.(0) to y.(2) do
                for j = y.(1) to y.(3) do
                         match x.(i).(j) with
                                 | Null -> raise (NullValue ("Null value at "^(string_of_int i)^","^(string_of_int j)));
                                 | Value t -> 
                                                begin 
                                                    if (!ans < t) then ans := t;
                                                end;
               done;
        done;
        x.(z.(0)).(z.(1)) <-Value !ans;
        x;;

let row_max (x:sheet) (y:range) (z:index)=
        for i = y.(0) to y.(2) do
                let ans = ref min_float in
                for j = y.(1) to y.(3) do
                         match x.(i).(j) with
                                 | Null -> raise (NullValue ("Null value at "^(string_of_int i)^","^(string_of_int j)));
                                 | Value t ->  begin 
                                                    if (!ans < t) then ans := t;
                                                end;
               done;
               x.(z.(0)+i-y.(0)).(z.(1)) <- Value !ans  
        done;
        x;;

let col_max (x:sheet) (y:range) (z:index)=
        for j = y.(1) to y.(3) do
                let ans = ref min_float in
                for i = y.(0) to y.(2) do
                         match x.(i).(j) with
                                 | Null -> raise (NullValue ("Null value at "^(string_of_int i)^","^(string_of_int j)));
                                 | Value t -> begin 
                                                    if (!ans < t) then ans := t;
                                              end;
               done;
               x.(z.(0)).(z.(1)+j-y.(1)) <- Value !ans;  
        done;
        x;;


let full_min (x:sheet) (y:range) (z:index)=
        let ans = ref max_float in
        for i = y.(0) to y.(2) do
                for j = y.(1) to y.(3) do
                         match x.(i).(j) with
                                 | Null -> raise (NullValue ("Null value at "^(string_of_int i)^","^(string_of_int j)));
                                 | Value t -> 
                                                begin 
                                                    if (!ans > t) then ans := t;
                                                end;
               done;
        done;
        x.(z.(0)).(z.(1)) <-Value !ans;
        x;;

let row_min (x:sheet) (y:range) (z:index)=
        for i = y.(0) to y.(2) do
                let ans = ref max_float in
                for j = y.(1) to y.(3) do
                         match x.(i).(j) with
                                 | Null -> raise (NullValue ("Null value at "^(string_of_int i)^","^(string_of_int j)));
                                 | Value t ->  begin 
                                                    if (!ans > t) then ans := t;
                                                end;
               done;
               x.(z.(0)+i-y.(0)).(z.(1)) <- Value !ans;  
        done;
        x;;

let col_min (x:sheet) (y:range) (z:index) =
        for j = y.(1) to y.(3) do
                let ans = ref max_float in
                for i = y.(0) to y.(2) do
                         match x.(i).(j) with
                                 | Null -> raise (NullValue ("Null value at "^(string_of_int i)^","^(string_of_int j)));
                                 | Value t ->  begin 
                                                    if (!ans > t) then ans := t;
                                                end;
               done;
               x.(z.(0)).(z.(1)+j-y.(1)) <- Value !ans;
        done;
        x;;

let add_const (x:sheet) (y:range) c (z:index) =
        let iof = z.(0) - y.(0) in
        let jof = z.(1) - y.(1) in
        for i = y.(0) to y.(2) do
                let i_new = i + iof in
                for j = y.(1) to y.(3) do
                         match x.(i).(j) with
                                 | Null -> raise (NullValue ("Null value at "^(string_of_int i)^","^(string_of_int j)));
                                 | Value t -> begin x.(i_new).(j+jof) <- Value (t +. c); end;
               done;
        done;
        x;;

let subt_const (x:sheet) (y:range) t (z:index) =
    add_const x y (-1.0*.t) z;;

let mult_const  (x:sheet) (y:range) c (z:index) =
        let iof = z.(0) - y.(0) in
        let jof = z.(1) - y.(1) in
        for i = y.(0) to y.(2) do
                let i_new = i + iof in
                for j = y.(1) to y.(3) do
                         match x.(i).(j) with
                                 | Null -> raise (NullValue ("Null value at "^(string_of_int i)^","^(string_of_int j)));
                                 | Value t -> begin x.(i_new).(j+jof) <- Value (t*.c); end;
               done;
        done;
        x;;


let div_const  (x:sheet) (y:range) t (z:index) =
        mult_const x y (1.0/.t) z;;

let subt_const1 (x:sheet) (y:range) c (z:index) =
        let iof = z.(0) - y.(0) in
        let jof = z.(1) - y.(1) in
        for i = y.(0) to y.(2) do
                let i_new = i + iof in
                for j = y.(1) to y.(3) do
                         match x.(i).(j) with
                                 | Null -> raise (NullValue ("Null value at "^(string_of_int i)^","^(string_of_int j)));
                                 | Value t -> begin x.(i_new).(j+jof) <- Value (c -. t); end;
               done;
        done;
        x;;

let div_const1  (x:sheet) (y:range) c (z:index) =
        let iof = z.(0) - y.(0) in
        let jof = z.(1) - y.(1) in
        for i = y.(0) to y.(2) do
                let i_new = i + iof in
                for j = y.(1) to y.(3) do
                         match x.(i).(j) with
                                 | Null -> raise (NullValue ("Null value at "^(string_of_int i)^","^(string_of_int j)));
                                 | Value t -> begin x.(i_new).(j+jof) <- Value (c/.t); end;
               done;
        done;
        x;;

let add_range (x:sheet) (y1:range) (y2:range) (z:index) = 
        let i_max = y1.(2) - y1.(0) in
        let j_max = y1.(3) - y1.(1) in
        for i = 0 to i_max do
                let i_new = i + z.(0) in
                for j = 0 to j_max do
                         match x.(i+y1.(0)).(j+y1.(1)),x.(i+y2.(0)).(j+y2.(1)) with
                                 | Value t1, Value t2 -> begin x.(i_new).(j+z.(1)) <- Value (t1 +. t2); end;
                                 | _ -> raise (NullValue ("Null value at "^(string_of_int i)^","^(string_of_int j)));
               done;
        done;
        x;;
        
 let subt_range (x:sheet) (y1:range) (y2:range) (z:index) = 
        let i_max = y1.(2) - y1.(0) in
        let j_max = y1.(3) - y1.(1) in
        for i = 0 to i_max do
                let i_new = i + z.(0) in
                for j = 0 to j_max do
                         match x.(i+y1.(0)).(j+y1.(1)),x.(i+y2.(0)).(j+y2.(1)) with
                                 | Value t1, Value t2 -> begin x.(i_new).(j+z.(1)) <- Value (t1 -. t2); end;
                                 | _ -> raise (NullValue ("Null value at "^(string_of_int i)^","^(string_of_int j)));
               done;
        done;
        x;;
let print (x:sheet) =
    for i = 0 to (length x) - 1 do
    for j = 0 to (length x.(0)) - 1 do
                     match x.(i).(j) with
                     | Value c -> print_string ((string_of_float c)^" ");
                     | Null -> print_string ("Null ");
    done;
    print_string "\n";
done;;
                              
let mult_range (x:sheet) (y1:range) (y2:range) (z:index) = 
        let i_max = y1.(2) - y1.(0) in
        let j_max = y1.(3) - y1.(1) in
        for i = 0 to i_max do
                let i_new = i + z.(0) in
                for j = 0 to j_max do
                         match x.(i+y1.(0)).(j+y1.(1)),x.(i+y2.(0)).(j+y2.(1)) with
                                 | Value t1, Value t2 -> begin x.(i_new).(j+z.(1)) <- Value (t1 *. t2); end;
                                 | _ -> raise (NullValue ("Null value at "^(string_of_int i)^","^(string_of_int j)));
               done;
        done;
        x;;
        
 let div_range (x:sheet) (y1:range) (y2:range) (z:index) = 
        let i_max = y1.(2) - y1.(0) in
        let j_max = y1.(3) - y1.(1) in
        for i = 0 to i_max do
                let i_new = i + z.(0) in
                for j = 0 to j_max do
                         match x.(i+y1.(0)).(j+y1.(1)),x.(i+y2.(0)).(j+y2.(1)) with
                                 | Value t1, Value t2 -> begin x.(i_new).(j+z.(1)) <- Value (t1 /. t2); end;
                                 | _ -> raise (NullValue ("Null value at "^(string_of_int i)^","^(string_of_int j)));
               done;
        done;
        x;;

let rec execute x1 n=
        match n with
        | (Zeroary (INDICE i),b)  -> begin 
                                    x1 := update !x1 i.(0) i.(1);
                                    match b with
                                    | Unary (a, Zeroary (RANGE r)) -> 
                                    x1 := update !x1 r.(2) r.(3);
                                    let x = !x1 in
                                    begin
                                                                        match a with
                                                                        | COUNT ->full_count x r i;
                                                                        | ROWCOUNT ->  row_count x r i;
                                                                        | COLCOUNT -> col_count x r i;
                                                                        | SUM -> full_sum x r i;
                                                                        | ROWSUM -> row_sum x r i;
                                                                        | COLSUM -> col_sum x r i;
                                                                        | AVG -> full_avg x r i;
                                                                        | ROWAVG -> row_avg x r i;
                                                                        | COLAVG -> col_avg x r i;
                                                                        | MIN -> full_min x r i;
                                                                        | ROWMIN -> row_min x r i;
                                                                        | COLMIN -> col_min x r i;
                                                                        | MAX -> full_max x r i;
                                                                        | ROWMAX -> row_max x r i;
                                                                        | COLMAX -> col_max x r i;
                                                                        | _ -> raise SytaxErrorLSP;
                                                                    end;
                                    | Binary (a, Zeroary (RANGE r), Zeroary (CONST c)) -> begin
                                    x1 := update !x1 r.(2) r.(3);
                                    let x = !x1 in
                                                                                            match a with
                                                                                            | ADD -> add_const x r c i; 
                                                                                            | DIV -> div_const x r c i;
                                                                                            | SUBT-> subt_const x r c i;
                                                                                            | MULT -> mult_const x r c i;
                                                                                            | _ -> raise SytaxErrorLSP;
                                                                                        end;

                                    | Binary (a, Zeroary (CONST c), Zeroary (RANGE r)) -> begin
                                    x1 := update !x1 r.(2) r.(3);
                                    let x = !x1 in
                                                                                            match a with
                                                                                            | ADD -> add_const x r c i; 
                                                                                            | DIV -> div_const1 x r c i;
                                                                                            | SUBT-> subt_const1 x r c i;
                                                                                            | MULT -> mult_const x r c i;
                                                                                            | _ -> raise SytaxErrorLSP;
                                                                                        end;
                                    | Binary (a, Zeroary (INDICE i2), Zeroary (RANGE r)) -> begin
                                    x1 := update !x1 r.(2) r.(3);
                                    let x = !x1 in
                                                                                            match x.(i2.(0)).(i2.(1)) with
                                                                                            | Null -> raise (NullValue "can't have null value in alu operations")
                                                                                            | Value c-> begin
                                                                                        match a with
                                                                                        | ADD -> add_const x r c i; 
                                                                                        | DIV -> div_const x r c i;
                                                                                        | SUBT-> subt_const x r c i;
                                                                                        | MULT -> mult_const x r c i;
                                                                                        | _ -> raise SytaxErrorLSP;
                                                                                        end;
                                                                                    end;

                                    | Binary (a,Zeroary (RANGE r),Zeroary (INDICE i2)) -> begin
                                    x1 := update !x1 r.(2) r.(3);
                                    let x = !x1 in
                                                                                            match x.(i2.(0)).(i2.(1)) with
                                                                                            | Null -> raise (NullValue "can't have null value in alu operations")
                                                                                            | Value c-> begin
                                                                                        match a with
                                                                                        | ADD -> add_const x r c i; 
                                                                                        | DIV -> div_const x r c i;
                                                                                        | SUBT-> subt_const x r c i;
                                                                                        | MULT -> mult_const x r c i;
                                                                                        | _ -> raise SytaxErrorLSP;
                                                                                        end;
                                                                                    end;

                                    | Binary (a, Zeroary (RANGE r1), Zeroary (RANGE r2)) -> begin
                                        x1 := update !x1 r1.(2) r1.(3);
                                        x1 := update !x1 r2.(2) r2.(3);
                                    let x = !x1 in
                                                                                            match a with
                                                                                            | ADD -> add_range x r1 r2 i; 
                                                                                            | DIV -> div_range x r1 r2 i;
                                                                                            | SUBT-> subt_range x r1 r2 i;
                                                                                            | MULT -> mult_range x r1 r2 i;
                                                                                            | _ -> raise SytaxErrorLSP;
                                                                                        end;
                                    | _ -> raise SytaxErrorLSP;
                                    end;
        | _ -> raise SytaxErrorLSP;;