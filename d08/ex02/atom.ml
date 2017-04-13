class virtual atom name symbol nb =
    object(self)
        val _name:string = name
        val _symbol:string = symbol
        val _nb:int = nb

        method to_string = (string_of_int _nb) ^ " " ^ _name ^  "(" ^ _symbol ^ ") "

        method getName = _name
        method getSymbol = _symbol
        method getNb = _nb

        method equals (other:atom) = (name = other#getName && symbol = other#getSymbol && nb = other#getNb)
    end

class hydrogen =
    object 
        inherit atom "Hydrogen" "H" 1
    end

class helium =
    object 
        inherit atom "Helium" "He" 2
    end

class lithium =
    object 
        inherit atom "Lithium" "Li" 3
    end

class beryllium =
    object 
        inherit atom "Béryllium" "Be" 4
    end

class boron =
    object 
        inherit atom "Boron" "B" 5
    end

class carbon =
    object 
        inherit atom "Carbon" "C" 6
    end

class nitrogen =
    object 
        inherit atom "Nitrogen" "N" 7
    end

class oxygen =
    object 
        inherit atom "Oxygen" "O" 8
    end

class fluorine =
    object 
        inherit atom "Fluorine" "F" 9
    end

class neon =
    object 
        inherit atom "Neon" "N" 10
    end
