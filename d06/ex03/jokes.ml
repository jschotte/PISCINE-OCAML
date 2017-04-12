let printArray arr =
    if Array.length arr > 0
    then print_endline (arr.(Random.int (Array.length arr)))
    else ()

let fillArray av =
    Random.self_init ();
        let fname = av.(1) in
        begin try
        let ic = open_in fname in
            let arr = Array.make 0 "" in
            let tmp = ref (arr) in
            begin try
                while true; do
                    tmp := Array.append !tmp [|input_line ic|];
                done
            with
            | Sys_error err -> Printf.printf "Error: %s\n" err
            | End_of_file   -> printArray !tmp    
            end;
            close_in ic
        with
        | Sys_error err -> Printf.printf "Error: %s" err
        end

let () =
    let av = Sys.argv in
    if Array.length av < 2
    then print_endline "Argument Error !"
    else fillArray av
