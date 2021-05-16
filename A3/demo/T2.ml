type e = 
        | N of float
        | Add 
        | Plus
        | Minus
        | Mul
        | Div

type node =
        Nul
        | Uniary of e
        | Binary of node*e*node

let rec depth n = 
        match n with
        | Nul -> 0
        | Uniary x -> 1
        | Binary (x,y,z) -> let dx = depth x in
                            let dz = depth z in
                            if (dx > dz) then dx + 1
                            else dz + 1;;
