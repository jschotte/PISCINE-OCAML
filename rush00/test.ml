module Value =
    struct
        type t = X | O | N

        let toString t = match t with
            | X     -> "X"
            | O     -> "0"
            | N     -> "-"
    
        let isFree t = if t = N
                       then true
                       else false
    end

module Map =
    struct
        type t = Value.t list

        let newMap () = [Value.N;Value.N;Value.N;Value.N;Value.N;Value.N;Value.N;Value.N;Value.N]

        let printMap t = 
            let rec print acc t = match t with
                | []            -> ()
                | head :: tail  -> print_string (Value.toString head ^ " ");
                                   if acc mod 3 = 0 && acc <> 0
                                   then print_newline ();
                                   print (acc + 1) tail
            in
            print 1 t

        let printline t line =
            let rec print acc t = match t with
                | []                    -> ""
                | hd::n1::n2::tail      -> if acc = line
                                            then (Value.toString hd) ^ " " ^ (Value.toString n1) ^ " " ^ (Value.toString n2)
                                            else print (acc + 1) tail
                | _                     -> ""
            in
            print 1 t
    end

module Game =
    struct
        type t = Map.t list

        let newGame () = [Map.newMap();Map.newMap();Map.newMap();Map.newMap();Map.newMap();Map.newMap();Map.newMap();Map.newMap();Map.newMap()]

        let printGame t = 
            print_endline "---------------------";
            let rec print t = match t with
                | []                        -> ()
                | head :: n1 :: n2 :: tail  -> print_endline ( (Map.printline (head) 1) ^ " | "  ^ (Map.printline (n1) 1) ^ " | " ^ (Map.printline (n2) 1));
                                               print_endline ( (Map.printline (head) 2) ^ " | "  ^ (Map.printline (n1) 2) ^ " | " ^ (Map.printline (n2) 2));
                                               print_endline ( (Map.printline (head) 3) ^ " | "  ^ (Map.printline (n1) 3) ^ " | " ^ (Map.printline (n2) 3));
                                               print_endline "---------------------";
                                               print tail
                | _                         -> ()
                                   
            in
            print t
    end


let () = 
    Game.printGame (Game.newGame ())







