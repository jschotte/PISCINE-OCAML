let () =
    let rec loop l = match l with
        | []            -> ()
        | head :: tail  -> print_string    ("Card: "
                                           ^ Value.toStringVerbose head
                                           ^ "("
                                           ^ Value.toString head
                                           ^ ") | Value: "
                                           ^ string_of_int (Value.toInt head));
                                           if head <> Value.T2
                                           then print_string (" | Previous " ^ Value.toString (Value.previous head));
                                           if head <> Value.As
                                           then print_string (" | Next " ^ Value.toString (Value.next head));
                                           print_newline ();
                           loop tail
    in
    loop Value.all
