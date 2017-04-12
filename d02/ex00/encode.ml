let encode l =
    let rec loop count l = match l with
    | []                -> []
    | head :: []          -> (count, head) :: []
    | head :: next :: tail -> if head = next then loop (count + 1) (next :: tail)
                                   else (count, head) :: loop 1 (next :: tail)
    in
    loop 1 l

let () =
    let rec print_encode f list = match list with
      | (n, v) :: l ->  print_char '(' ;
                        print_int n ;
                        print_string ", " ;
                        f v ;
                        print_string ") " ;
                        print_encode f l
      | [] -> print_char '\n'
    in
    print_encode print_string (encode ["1"; "1"; "2"; "3"; "2"]);
    print_encode print_int (encode [42; 42; 0]);
    print_encode print_float (encode [1.]);
    print_encode print_int (encode [])


