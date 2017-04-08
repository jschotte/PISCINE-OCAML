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
        
       (*let replaceValue x value oldmap =
           let rec findvalue acc x value oldmap newmap= match oldmap with
            | []            -> []
            | head :: tail  -> if acc = x
                               then newmap @ value
                               else newmap @ tail;
                               findvalue (acc + 1) x value tail newmap
            in
            findvalue 1 x value oldmap []*)

        let replace_in_map nb_case value t =  List.mapi (fun i x -> if i = nb_case - 1 then value else x) t
       

       let verification t = match t with
                            | a :: b :: c
                        ::    d :: e :: f
                        ::    g :: h :: i 
                        :: []
                        ->
                            if a = b && b = c then a
(* Honrizontale Check *)    else if d = e && e = f then d
                            else if g = h && h = i then g
 
                            else if a = d && d = g then a
(*   Vertical Check   *)    else if b = e && e = h then b
                            else if c = f && f = i then c

(*    Cross Check     *)    else if a = e && e = i then a
                            else if c = e && e = g then c

(*    NUll NATCH      *)    else Value.N
                        | _ -> Value.N
    end

module Game =
    struct
        type t = {map: Map.t list}

        let getMap t = t.map

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
           
       let replace_in_map nb_map nb_case value t = List.mapi (fun i x -> if i = nb_map - 1 then (Map.replace_in_map nb_case value x) else x) t
    end


let () = 
    let game = Game.newGame () in
    Game.printGame game;
    let ng = Game.replace_in_map 2 4 Value.X game in
    Game.printGame ng





    






