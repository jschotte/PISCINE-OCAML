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

let verif_draw t =
    let rec count acc l = match l with
        | []            -> acc
        | head :: tail  -> if Value.isFree head
                           then count (acc + 1) tail
                           else count acc tail
    in
    count 0 t

let replace_in_map nb_map nb_case value t =
    if verif_draw t = 1 && nb_map <> 10
    then List.mapi (fun i x -> if i = nb_case - 1 then value else value) t 
    else List.mapi (fun i x -> if i = nb_case - 1 then value else x) t 

let fillvalue value t = List.mapi (fun i x -> value) t

let verif_case y t =
    if List.nth t (y-1) <> Value.N
    then false
    else true


let verificationIA t = match t with
        | a :: b :: c :: d :: e :: f :: g :: h :: i :: tail -> if a = b && a = Value.O && c = Value.N then 3 
                                                             else if a = c && c = Value.O && b = Value.N then 2 
                                                             else if b = c && c = Value.O && a = Value.N then 1 
                                                             else if d = e && e = Value.O && f = Value.N then 6 
                                                             else if d = f && f = Value.O && e = Value.N then 5 
                                                             else if e = f && f = Value.O && d = Value.N then 4 
                                                             else if g = h && h = Value.O && i = Value.N then 7 
                                                             else if g = i && i = Value.O && h = Value.N then 8 
                                                             else if h = i && i = Value.O && g = Value.N then 9 

                                                             else if a = d && d = Value.O && g = Value.N then 7 
                                                             else if a = g && g = Value.O && d = Value.N then 4 
                                                             else if d = g && d = Value.O && a = Value.N then 1 
                                                             else if b = e && e = Value.O && h = Value.N then 8 
                                                             else if b = h && h = Value.O && e = Value.N then 5 
                                                             else if h = e && e = Value.O && b = Value.N then 2 

                                                             else if c = f && f = Value.O && i = Value.N then 9 
                                                             else if c = i && i = Value.O && f = Value.N then 6 
                                                             else if i = f && f = Value.O && c = Value.N then 3 
                                                                    
                                                             else if a = e && e = Value.O && i = Value.N then 9 
                                                             else if a = i && i = Value.O && e = Value.N then 5 
                                                             else if i = e && e = Value.O && a = Value.N then 1 

                                                             else if c = e && e = Value.O && g = Value.N then 7 
                                                             else if c = g && g = Value.O && e = Value.N then 5 
                                                             else if g = e && e = Value.O && c = Value.N then 3 
                                                            
                                                             else if a = b && a = Value.X && c = Value.N then 3 
                                                             else if a = c && c = Value.X && b = Value.N then 2 
                                                             else if b = c && c = Value.X && a = Value.N then 1 
                                                             else if d = e && e = Value.X && f = Value.N then 6 
                                                             else if d = f && f = Value.X && e = Value.N then 5 
                                                             else if e = f && f = Value.X && d = Value.N then 4 
                                                             else if g = h && h = Value.X && i = Value.N then 7 
                                                             else if g = i && i = Value.X && h = Value.N then 8 
                                                             else if h = i && i = Value.X && g = Value.N then 9 

                                                             else if a = d && d = Value.X && g = Value.N then 7 
                                                             else if a = g && g = Value.X && d = Value.N then 4 
                                                             else if d = g && d = Value.X && a = Value.N then 1 
                                                             else if b = e && e = Value.X && h = Value.N then 8 
                                                             else if b = h && h = Value.X && e = Value.N then 5 
                                                             else if h = e && e = Value.X && b = Value.N then 2 

                                                             else if c = f && f = Value.X && i = Value.N then 9 
                                                             else if c = i && i = Value.X && f = Value.N then 6 
                                                             else if i = f && f = Value.X && c = Value.N then 3 
                                                                    
                                                             else if a = e && e = Value.X && i = Value.N then 9 
                                                             else if a = i && i = Value.X && e = Value.N then 5 
                                                             else if i = e && e = Value.X && a = Value.N then 1 

                                                             else if c = e && e = Value.X && g = Value.N then 7 
                                                             else if c = g && g = Value.X && e = Value.N then 5 
                                                             else if g = e && e = Value.X && c = Value.N then 3

                                                            

    (*    NUll NATCH      *)                                 else if a = Value.N then 1
                                                             else if c = Value.N then 3
                                                             else if b = Value.N then 2
                                                             else if e = Value.N then 5
                                                             else if f = Value.N then 6
                                                             else if d = Value.N then 4
                                                             else if h = Value.N then 8
                                                             else if i = Value.N then 9
                                                             else if g = Value.N then 7
                                                           else (-1)
       | _                                                 -> 10






