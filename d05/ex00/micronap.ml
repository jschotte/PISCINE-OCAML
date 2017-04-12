let my_sleep () = Unix.sleep 1

let () =
    let s = Sys.argv in
    try
        let nb = ref (int_of_string s.(1)) in
        while !nb > 0 do
            my_sleep ();
            decr nb
        done
    with
    | Sys_error err ->  print_endline err
    | _ -> print_endline "Argument Error !"
