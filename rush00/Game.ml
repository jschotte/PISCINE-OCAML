type t = {map: Map.t list}

let getMap t = t.map

let newGame () = [Map.newMap();Map.newMap();Map.newMap();Map.newMap();Map.newMap();Map.newMap();Map.newMap();Map.newMap();Map.newMap();Map.newMap()]

let printEndMap t =
    print_endline (Map.printline (List.nth t 9) 1);
    print_endline (Map.printline (List.nth t 9) 2);
    print_endline (Map.printline (List.nth t 9) 3)

let printGame t = 
    print_endline "---------------------";
    let rec print l = match l with
        | []                        -> ()
        | head :: n1 :: n2 :: tail  -> print_endline ( (Map.printline (head) 1) ^ " | "  ^ (Map.printline (n1) 1) ^ " | " ^ (Map.printline (n2) 1));
                                       print_endline ( (Map.printline (head) 2) ^ " | "  ^ (Map.printline (n1) 2) ^ " | " ^ (Map.printline (n2) 2));
                                       print_endline ( (Map.printline (head) 3) ^ " | "  ^ (Map.printline (n1) 3) ^ " | " ^ (Map.printline (n2) 3));
                                       print_endline "---------------------";
                                       print tail
        | _                         -> ()
                           
    in
    print t
   
let replace_in_map nb_map nb_case value t =
    List.mapi (fun i x -> if i = nb_map - 1 then (Map.replace_in_map nb_map nb_case value x) else x) t

let check nb_map t =
  Map.isWin (List.nth t (nb_map - 1))

let verif_input x y t =
   if x > 9 || x < 0 || y > 9 || y < 0 || Map.isWin (List.nth t (x - 1)) <> Value.N
   then false
   else
        Map.verif_case y (List.nth t (x - 1))

let verif_draw t = 
    let rec checkall acc l = match l with
    | []                    -> false
    | last :: []            ->  if acc = 0
                                then true
                                else false
    | head :: tail  -> if Map.isWin head <> Value.N
                       then checkall acc tail
                       else checkall (acc + Map.verif_draw head) tail
    in
    checkall 0 t

let getFirstMapFree t =
    let rec find acc l = match l with
        | []                -> -1
        | head :: tail      ->  if Map.isWin head = Value.N && Map.verif_draw head > 0
                                then acc
                                else find (acc + 1) tail
    in
    find 1 t
