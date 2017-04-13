let () =
    let meth = new Alkane.methane in
    let eth = new Alkane.ethane in
    let prop = new Alkane.propane in
    let but = new Alkane.butane in
    let pen  = new Alkane.pentane in
    let hex  = new Alkane.hexane in
    let hept = new Alkane.heptane in
    let oct  = new Alkane.octane in
    let non = new Alkane.nonane in
    let dec = new Alkane.decane in
    let und = new Alkane.undecane in
    let dod = new Alkane.dodecane in
    print_endline meth#to_string;
    print_endline eth#to_string;
    print_endline prop#to_string;
    print_endline but#to_string;
    print_endline pen#to_string;
    print_endline hex#to_string;
    print_endline hept#to_string;
    print_endline oct#to_string;
    print_endline non#to_string;
    print_endline dec#to_string;
    print_endline und#to_string;
    print_endline dod#to_string
