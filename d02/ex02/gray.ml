(* n lsr/lsl m : shift n by m bits *)

let reverse c =
        if c = '0'
        then '1'
        else '0'

let gray n =
    let s = String.make n '0' in
    let rec loop x =
        print_endline s;
        s.[x] <- reverse s.[x];
        if x - 1 >= 0
        then loop (x - 1)
    in
    loop (String.length s -1) 

let () =
    gray 1;
    print_newline ();
    gray 2
