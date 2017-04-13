let findName x = match x with 
        | 1  -> "Methane"
        | 2  -> "Ethane"
        | 3  -> "Propane"
        | 4  -> "Butane"
        | 5  -> "Pentane"
        | 6  -> "Hexane"
        | 7  -> "Heptane"
        | 8  -> "Octane"
        | 9  -> "Nonane"
        | 10 -> "Decane"
        | 11 -> "Undecane"
        | 12 -> "Dodecane"
        | _  -> ""

let findFormula n =
    let x = [] in 
    let rec addCarbon acc x = match acc with
        | 0     -> x
        | _     -> addCarbon (acc - 1) (new Atom.carbon :: x)
    in
    let y = addCarbon n x in
    let rec addHydrogen acc y = match acc with
        | 0     -> y
        | _     -> addHydrogen (acc - 1) (new Atom.hydrogen :: y)
    in
    addHydrogen (2*n+2) y


class virtual alkane n = 
    object(self)
        inherit Molecule.molecule (findName n) (findFormula n)
    end

class methane =
    object
        inherit alkane 1
    end

class ethane =
    object
        inherit alkane 2
    end

class propane =
    object
        inherit alkane 3
    end

class butane =
    object
        inherit alkane 4
    end

class pentane =
    object
        inherit alkane 5
    end

class hexane =
    object
        inherit alkane 6
    end

class heptane =
    object
        inherit alkane 7
    end

class octane =
    object
        inherit alkane 8
    end

class nonane =
    object
        inherit alkane 9
    end

class decane =
    object
        inherit alkane 10
    end

class undecane =
    object
        inherit alkane 11
    end

class dodecane =
    object
        inherit alkane 12
    end
