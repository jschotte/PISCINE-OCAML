class doctor name age =
    object (self)
        val _name:string = name
        val mutable _age:int = age
        val _sidekick = new People.people name
        val mutable _hp:int = 100

        method to_string = _name ^ " : " ^ (string_of_int _age) ^ " years old, "
        ^ (string_of_int _hp) ^ " hp, " ^ (_sidekick#to_string)
        
        method talk = print_endline "Hi! I’m the Doctor!"
        
        method travel_in_time start arrival = _age <- _age + (arrival - start);
        print_endline "______(_@_)_____";
        print_endline "| POLICE    BOX |";
        print_endline "|_______________|";
        print_endline "| _____ | _____ |";
        print_endline "| |###| | |###| |";
        print_endline "| |###| | |###| |"; 
        print_endline "| _____ | _____ |";   
        print_endline "| || || | || || |";
        print_endline "| ||_|| | ||_|| |"; 
        print_endline "| _____ |$_____ |";  
        print_endline "| || || | || || |";
        print_endline "| ||_|| | ||_|| |"; 
        print_endline "| _____ | _____ |";
        print_endline "| || || | || || |"; 
        print_endline "| ||_|| | ||_|| |";         
        print_endline "|       |       |";      
        print_endline "*****************"

        method use_sonic_screwdriver = print_endline "Whiiiiwhiiiwhiii Whiiiiwhiiiwhiii Whiiiiwhiiiwhiii"

        method private regenerate = _hp <- 100

        initializer print_endline "A new doctor was born"
    end
