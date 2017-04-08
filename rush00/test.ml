let is_digit c = c >= '0' && c <= '9'

let ft_string_all pred str =
    let l = (String.length str) - 1 in
    let rec loop i =
        if i > l
        then true
        else if pred (String.get str (i)) = false && String.get str (i) <> ' '
        then false
        else
        loop (i + 1);
    in
    loop 0

module Value =
    struct
        type t = X | O | N

        let toString t = match t with
            | X     -> "X"
            | O     -> "O"
            | N     -> "-"
    
        let isFree t = if t = N
                       then true
                       else false

        let verification t = match t with
        | a :: b :: c :: d :: e :: f :: g :: h :: i :: [] -> if a = b && b = c && a <> N then a
    (* Honrizontale Check *)                                 else if d = e && e = f && d <> N then d
                                                             else if g = h && h = i && g <> N then g
                                                             else if a = d && d = g && a <> N then a
    (*   Vertical Check   *)                                 else if b = e && e = h && b <> N then b
                                                             else if c = f && f = i && c <> N then c
    (*    Cross Check     *)                                 else if a = e && e = i && a <> N then a
                                                             else if c = e && e = g && c <> N then c
    (*    NUll NATCH      *)                                 else N
       | _                                                 -> N
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

        let isWin t = Value.verification t
       
        

        let printline t line = 
            let rec print acc l = match l with
                | []                    -> ""
                | hd::n1::n2::tail      -> if acc = line
                                           then
                                           begin
                                               if isWin t = Value.N
                                               then (Value.toString hd) ^ " " ^ (Value.toString n1) ^ " " ^ (Value.toString n2)
                                               else if isWin t = Value.X
                                               then
                                                   begin
                                                       if line = 1
                                                       then "\\   /"
                                                       else if line = 2
                                                       then "  X  "
                                                       else "/   \\"
                                                   end
                                               else
                                                   begin
                                                       if line = 1
                                                       then "/ - \\"
                                                       else if line = 2
                                                       then "|   |"
                                                       else "\\ - /"
                                                   end
                                           end
                                           else print (acc + 1) tail
                | _                     -> ""
            in
            print 1 t
        
        let replace_in_map nb_case value t =  List.mapi (fun i x -> if i = nb_case - 1 then value else x) t 


        let verif_case y t =
            if List.nth t (y-1) <> Value.N
            then false
            else true
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
       
       let check nb_map t =
           Map.isWin (List.nth t (nb_map - 1))
       
       let verif_input x y t =
           if x > 9 || x < 0 || y > 9 || y < 0 || Map.isWin (List.nth t (x - 1)) <> Value.N
           then false
           else
                Map.verif_case y (List.nth t (x - 1))
    end


let () =
    let v = Value.X in
    print_endline "Entrez le nom du joueur 1";
    let name1 = read_line () in
    print_endline "Entrez le nom du joueur 2 (IA)";
    let name2 = read_line () in
    let game = Game.newGame () in
    Game.printGame game;
    let rec whileGame game v =
        if v = Value.O && name2 = "IA"
        then
            begin
        if v = Value.X
        then print_endline ("Au tour de " ^ name1 ^ " (X) de jouer, veuillez entrer les coordonnees ")
        else
        print_endline ("Au tour de " ^ name2 ^ " (O) de jouer, veuillez entrer les coordonnees ");
        let input = read_line () in
        if ft_string_all is_digit input = false         (* VERIF TYPE INPUT *)
        then 
            begin
                print_endline "Invalid params";
                whileGame game v;
            end;
        let split = String.split_on_char ' ' input in
        if List.length split < 2 || (String.length (List.nth (split) 0) <= 0)
                                 || (String.length (List.nth (split) 0) >= 2)
                                 || (String.length (List.nth (split) 1) <= 0)
                                 || (String.length (List.nth (split) 1) >= 2)
        then 
            begin
                print_endline "Invalid params";
                whileGame game v;
            end;
        let x = int_of_string (List.hd split) in
        let y = int_of_string (List.nth split 1) in 
        if Game.verif_input x y game = false             (* VERIF VALUE INPUT *)
        then 
            begin
                print_endline "Invalid params";
                whileGame game v;
            end;
        let ng = Game.replace_in_map x y v game in
        if Game.check x ng = Value.X
        then print_endline ("Le joueur " ^ name1 ^ " (" ^ (Value.toString v) ^ ") a complete la grille " ^ string_of_int x)
        else if Game.check x ng = Value.O 
            then print_endline ("Le joueur " ^ name2 ^ " (" ^ (Value.toString v) ^ ") a complete la grille " ^ string_of_int x);
        Game.printGame ng;
        if v = Value.X
        then whileGame ng Value.O
        else whileGame ng Value.X;
            end

        else
            begin
        if v = Value.X
        then print_endline ("Au tour de " ^ name1 ^ " (X) de jouer, veuillez entrer les coordonnees ")
        else
        print_endline ("Au tour de " ^ name2 ^ " (O) de jouer, veuillez entrer les coordonnees ");
        let input = read_line () in
        if ft_string_all is_digit input = false         (* VERIF TYPE INPUT *)
        then 
            begin
                print_endline "Invalid params";
                whileGame game v;
            end;
        let split = String.split_on_char ' ' input in
        if List.length split < 2 || (String.length (List.nth (split) 0) <= 0)
                                 || (String.length (List.nth (split) 0) >= 2)
                                 || (String.length (List.nth (split) 1) <= 0)
                                 || (String.length (List.nth (split) 1) >= 2)
        then 
            begin
                print_endline "Invalid params";
                whileGame game v;
            end;
        let x = int_of_string (List.hd split) in
        let y = int_of_string (List.nth split 1) in 
        if Game.verif_input x y game = false             (* VERIF VALUE INPUT *)
        then 
            begin
                print_endline "Invalid params";
                whileGame game v;
            end;
        let ng = Game.replace_in_map x y v game in
        if Game.check x ng = Value.X
        then print_endline ("Le joueur " ^ name1 ^ " (" ^ (Value.toString v) ^ ") a complete la grille " ^ string_of_int x)
        else if Game.check x ng = Value.O 
            then print_endline ("Le joueur " ^ name2 ^ " (" ^ (Value.toString v) ^ ") a complete la grille " ^ string_of_int x);
        Game.printGame ng;
        if v = Value.X
        then whileGame ng Value.O
        else whileGame ng Value.X;
            end
    in
    whileGame game v




    






