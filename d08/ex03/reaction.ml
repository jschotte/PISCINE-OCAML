class virtual reaction (startlst: (Molecule.molecule * int) list) (endlst: (Molecule.molecule * int) list) = 
    object(self)

        method virtual get_start: (Molecule.molecule * int) list
        method virtual get_result: (Molecule.molecule * int) list

        method virtual balance: reaction
        method virtual is_balance: bool
    end
