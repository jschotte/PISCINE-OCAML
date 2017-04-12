let () =
    let rec loop l = match l with
    | []            -> ()
    | head :: tail  -> print_endline (Color.toStringVerbose head);
                       print_endline (Color.toString head);
                       loop tail
    in
    loop Color.all
