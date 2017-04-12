class ['a] army (arm:'a list) =
    object(self)
        val mutable _members = arm

        method add n = _members <- n :: _members
        
        method delete = if List.length _members >= 1
                        then _members <- List.tl _members 
    end
