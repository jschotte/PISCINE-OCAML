let print_proj (elem:App.project) = match elem with
        | (x, y, z) -> print_endline (x ^ "  " ^ y ^ "  " ^ (string_of_int z))



let () = 
    let proj1:App.project = ("Test 1","",42) in
    let proj2:App.project = ("Test 2","",84) in
    let comb:App.project = App.combine proj1 proj2 in 
    print_proj comb;
    let comb:App.project = App.combine proj2 proj2 in 
    print_proj comb
