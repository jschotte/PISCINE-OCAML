let () =
    let h = new Atom.hydrogen in
    let h2 = new Atom.hydrogen in
    let he = new Atom.helium in
    let li = new Atom.lithium in
    let be = new Atom.beryllium in
    let b = new Atom.boron in
    let c = new Atom.carbon in
    let n = new Atom.nitrogen in
    let o = new Atom.oxygen in
    print_endline h#to_string;
    print_endline he#to_string;
    print_endline li#to_string;
    print_endline be#to_string;
    print_endline b#to_string;
    print_endline c#to_string;
    print_endline n#to_string;
    print_endline o#to_string;
    print_endline (string_of_bool(h#equals b));
    print_endline (string_of_bool(h#equals h2))
