let () =
    let water = new Molecule.water in
    let carb_di = new Molecule.carbon_dio in
    let tnt = new Molecule.tnt in
    let sugar = new Molecule.sugar in
    let glucose = new Molecule.glucose in
    let methane = new Molecule.methane in
    let amonia = new Molecule.amonia in
    print_endline water#to_string;
    print_endline carb_di#to_string;
    print_endline tnt#to_string;
    print_endline sugar#to_string;
    print_endline glucose#to_string;
    print_endline methane#to_string;
    print_endline amonia#to_string;
    print_endline (string_of_bool (sugar#equals methane));
    print_endline (string_of_bool (sugar#equals sugar))
