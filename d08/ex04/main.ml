let () =
    print_endline "----------------------"; 
    print_endline "Reaction Methane:";
    let reac = new Reaction.alkane_combustion 
    [(new Alkane.methane, 1); (new Molecule.dioxygen, 1)]
    [(new Molecule.carbon_dio, 1); (new Molecule.water, 1)] in
    print_endline reac#to_string;
    print_endline reac#balance#to_string;
    print_endline "----------------------"; 

    print_newline();

    print_endline "----------------------"; 
    print_endline "Reaction Propane:";
    let other = new Reaction.alkane_combustion
    [(new Alkane.propane, 1); (new Molecule.dioxygen, 1)]
    [(new Molecule.carbon_dio, 1); (new Molecule.water, 1)] in
    print_endline (other#to_string  ^ "|" ^ (string_of_bool other#is_balanced));
    print_endline other#balance#to_string; 
    print_endline "----------------------"; 
    
    print_newline();
   
    print_endline "----------------------"; 
    print_endline "Reaction Butane:";
  let but = new Reaction.alkane_combustion
    [(new Alkane.butane, 1); (new Molecule.dioxygen, 1)]
    [(new Molecule.carbon_dio, 1); (new Molecule.water, 1)] in
    print_endline but#to_string;
    print_endline but#balance#to_string;
    print_endline "----------------------"; 
    
    print_newline();
  
    print_endline "----------------------"; 
    print_endline "Reaction Ethane:";
  let eth = new Reaction.alkane_combustion
    [(new Alkane.ethane, 1); (new Molecule.dioxygen, 1)]
    [(new Molecule.carbon_dio, 1); (new Molecule.water, 1)] in
    print_endline (eth#to_string  ^ " | Is balanced: " ^ string_of_bool (eth#is_balanced));
   print_endline ( eth#balance#to_string ^ " | Is balanced: " ^ string_of_bool (eth#balance#is_balanced));
 
    print_endline "----------------------" 
