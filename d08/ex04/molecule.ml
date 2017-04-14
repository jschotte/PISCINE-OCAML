let gethill atoms =
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

class virtual molecule name atoms =
    object(self)
        val _name:string = name
        val _formula:string = gethill atoms
        val _atoms:(Atom.atom list) = atoms

        method getName = _name
        method getFormula = _formula
        method getAtoms = _atoms
        method to_string = _name ^ ": " ^ _formula
        method to_formstring = _formula

        method equals (other:molecule) = (_name = other#getName && _formula =
            other#getFormula)
    end

class water = 
    object
        inherit molecule "Water" [new Atom.oxygen; new Atom.hydrogen; new Atom.hydrogen]
    end

class tnt =
    object 
        inherit molecule "Trinitrotoluène" [new Atom.nitrogen; new Atom.nitrogen; new Atom.nitrogen;
                                            new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen;
                                            new Atom.hydrogen; new Atom.hydrogen; new Atom.oxygen;
                                            new Atom.oxygen;new Atom.oxygen;new Atom.oxygen;new Atom.oxygen;
                                            new Atom.oxygen;new Atom.carbon;new Atom.carbon;new Atom.carbon;
                                            new Atom.carbon;new Atom.carbon;new Atom.carbon;new Atom.carbon]
    end

class carbon_dio = 
    object
        inherit molecule "Carbon dioxyde" [new Atom.oxygen; new Atom.carbon; new Atom.oxygen;]
    end

class sugar =
    object 
        inherit molecule "Sugar" [new Atom.carbon;new Atom.carbon;
        new Atom.carbon;new Atom.carbon;new Atom.carbon;new Atom.carbon;
        new Atom.carbon;new Atom.carbon;new Atom.carbon;new Atom.carbon;
        new Atom.carbon;new Atom.carbon;new Atom.hydrogen;new Atom.hydrogen;
        new Atom.hydrogen;new Atom.hydrogen;new Atom.hydrogen;new Atom.hydrogen;
        new Atom.hydrogen;new Atom.hydrogen;new Atom.hydrogen;new Atom.hydrogen;
        new Atom.hydrogen;new Atom.hydrogen;new Atom.hydrogen;new Atom.hydrogen;
        new Atom.hydrogen;new Atom.hydrogen;new Atom.hydrogen;new Atom.hydrogen;
        new Atom.hydrogen;new Atom.hydrogen;new Atom.hydrogen;new Atom.hydrogen;
        new Atom.oxygen;new Atom.oxygen;new Atom.oxygen;new Atom.oxygen;
        new Atom.oxygen;new Atom.oxygen;new Atom.oxygen;new Atom.oxygen;
        new Atom.oxygen;new Atom.oxygen;new Atom.oxygen]
    end
  
class glucose =
   object
       inherit molecule "Glucose" [new Atom.oxygen;new Atom.oxygen;
       new Atom.oxygen;new Atom.oxygen;new Atom.oxygen;new Atom.oxygen; 
       new Atom.carbon;new Atom.carbon;new Atom.carbon;new Atom.carbon;
       new Atom.carbon;new Atom.carbon;new Atom.hydrogen;new Atom.hydrogen;
       new Atom.hydrogen;new Atom.hydrogen;new Atom.hydrogen;new Atom.hydrogen;
       new Atom.hydrogen;new Atom.hydrogen;new Atom.hydrogen;new Atom.hydrogen;
       new Atom.hydrogen;new Atom.hydrogen]
   end 

class methane =
    object
        inherit molecule "Methane" [new Atom.carbon; new Atom.hydrogen;new
        Atom.hydrogen;new Atom.hydrogen;new Atom.hydrogen]
    end

class amonia =
    object
        inherit molecule "Amonia" [new Atom.nitrogen; new Atom.hydrogen;new
        Atom.hydrogen;new Atom.hydrogen]
    end

class dioxygen =
    object
        inherit molecule "Dioxygene" [new Atom.oxygen; new Atom.oxygen]
    end
