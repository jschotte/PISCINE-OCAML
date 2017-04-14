class virtual reaction (startlst: (Molecule.molecule * int) list) (endlst: (Molecule.molecule * int) list) = 
    object(self)

        method virtual get_start: (Molecule.molecule * int) list
        method virtual get_result: (Molecule.molecule * int) list

        method virtual balance: reaction
        method virtual is_balanced: bool
        method virtual to_string: string
    end

let gethill (atoms: Atom.atom list) =
    let x = List.sort (fun i j -> if i#getNb = 6 then -1
                                  else if (i#getNb = 1 && j#getNb <> 6) then -1
                                  else compare i#getSymbol j#getSymbol) atoms in
    let rec getform acc s l = match l with
     | []                    -> s
     | head :: []            -> if acc > 1 then s ^ head#getSymbol ^ (string_of_int acc)
                                else s ^ head#getSymbol
     | head :: next :: tail  -> if head#getNb = next#getNb then getform (acc + 1) s (next :: tail)
                                else if acc > 1 then getform 1 (s ^ head#getSymbol ^ (string_of_int acc)) (next :: tail)
                                else getform 1 (s ^ head#getSymbol) (next :: tail)
    in
    getform 1 "" x

let get_all_atoms lst =
    let tmp = [] in
    let rec fuse tmp l = match l with
        | []            -> tmp
        | (x, y) :: tail  -> if (y = 1) then fuse (x#getAtoms @ tmp) tail
                             else fuse (x#getAtoms @ tmp) ((x, y - 1) :: tail)
    in
    let all = fuse tmp lst
    in let s = gethill all in
    s

let print_molecules lst =
    let rec print s l = match l with
    | []                -> s
    | (x, y) :: []      -> if y <> 1 then (s ^ (string_of_int y) ^ "(" ^ x#to_formstring ^ ")")
                           else (s ^ x#to_formstring ^ " ")
    | (x, y) :: tail    -> if y <> 1 then print (s ^ (string_of_int y) ^ "(" ^ x#to_formstring ^ ") + ") tail
                           else print (s ^ x#to_formstring ^ " + ") tail
    in
    print "" lst

let change start index add =
            let rec loop acc tmp l = match l with
                | []                -> tmp
                | (x, y) :: tail    ->  if acc = index then loop (acc + 1)((x,add)::tmp) tail
                                       else loop (acc + 1) ((x,y) :: tmp) tail
           in
            loop 0 [] start

class alkane_combustion startlst endlst =
    object(self)
        inherit reaction startlst endlst

        method is_balanced = get_all_atoms startlst = get_all_atoms endlst
        
        method get_start = if self#is_balanced = false
                           then raise (invalid_arg "Reaction is not balanced")
                           else startlst
        method get_result = if self#is_balanced = false
                           then raise (invalid_arg "Reaction is not balanced")
                           else endlst
       
        method balance = 
            let gets l = match l with
            | []                -> []
            | (x, y) :: tail    -> x#getAtoms
            in
            let s = (gets startlst) in
            let rec findAtom l acc atom = match l with
                | []            -> acc
                | head :: tail  -> if head#equals atom then findAtom tail (acc + 1) atom 
                                   else findAtom tail acc atom
            in
            let x = findAtom s 0 (new Atom.carbon) in
            let y = findAtom s 0 (new Atom.hydrogen) in
            let res = x + (y/4) in
            let res2 = (y/2) in
            let res3 = x in
            let res4 = (x*2) + ((y*2)/4) in
            let mo = y mod 4 in
            let rec loop elem x y = match elem#is_balanced with
                | true  -> elem
                | false -> if (mo = 0)
                           then  (new alkane_combustion (change startlst 1 (res))(change (change endlst 0 (res3)) 0 (res2)))
                           else  (new alkane_combustion (change (change startlst 1 (res4)) 1 (2)) (change (change endlst 0 (2*res3)) 0 (2*res2)))
            in
            loop (new alkane_combustion startlst endlst) 0 0
            
        method to_string = (print_molecules startlst) ^ " --> " ^ (print_molecules endlst)
    end


