module App =
    struct
        type project = string * string * int
        
        let zero = (("", "", 0):project)
        
        let combine ((s1, t1, x1):project) ((s2, t2, x2):project) =
            let res = (x1+x2)/2 in
            if res >= 80
            then ((s1 ^ s2, "sucess", res):project)
            else ((s1 ^ s2, "failed", res):project)
        
        let fail ((s, t, x):project) = ((s, "failed", 0):project)
        let sucess ((s, t, x):project) = ((s, "sucess", 80):project)
    end

let print_proj (elem:App.project) = match elem with
        | (x, y, z) -> print_endline (x ^ "  " ^ y ^ "  " ^ (string_of_int z))



let () = 
    let proj1:App.project = ("Test 1","",42) in
    let proj2:App.project = ("Test 2","",84) in
    let comb:App.project = App.combine proj1 proj2 in 
    print_proj comb;
    print_string ""
