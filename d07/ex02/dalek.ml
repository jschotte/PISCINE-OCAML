class dalek =
    object(self)
       
        val _name = Random.self_init();
                    "Dalek" ^ (String.init 3 (fun i -> if i = 0
                                                       then char_of_int (int_of_char 'A' + Random.int 26)
                                                       else char_of_int (int_of_char 'a' + Random.int 26)))
        val _hp:int = 100
        val mutable _shield:bool = true


        method to_string = _name ^ " " ^  (string_of_int _hp) ^ " " ^ (string_of_bool _shield)
        
        method talk = Random.self_init();
                      let tab = [|"Explain! Explain!";"Exterminate! Exterminate!";"I obey!";"You are the Doctor! You are the enemy of the Daleks!"|] in
                       print_endline (tab.(Random.int (Array.length tab)))

        method exterminate (people:People.people) = if _shield = false then _shield <- true
                                                    else _shield <- false;
                                                    people#die

        method die = print_endline "Emergency Temporal Shift!"
    end
