let () = 
    print_endline (string_of_bool (Card.isSpade (Card.newCard Card.Value.As Card.Color.Spade)));
    print_endline (string_of_bool (Card.isSpade (Card.newCard Card.Value.As Card.Color.Heart)));
    print_endline (string_of_bool (Card.isOf (Card.newCard Card.Value.As Card.Color.Diamond) Card.Color.Diamond));
    print_endline (string_of_bool (Card.isOf (Card.newCard Card.Value.As Card.Color.Club) Card.Color.Diamond));
    print_endline ( Card.toString (Card.max (Card.newCard Card.Value.As Card.Color.Spade) (Card.newCard Card.Value.Jack Card.Color.Diamond)));
    print_endline ( Card.toString (Card.min (Card.newCard Card.Value.As Card.Color.Spade) (Card.newCard Card.Value.Jack Card.Color.Diamond)));
    print_endline ( string_of_int (Card.compare (Card.newCard Card.Value.As Card.Color.Spade) (Card.newCard Card.Value.As Card.Color.Diamond)));
    print_endline ( string_of_int (Card.compare (Card.newCard Card.Value.King Card.Color.Spade) (Card.newCard Card.Value.As Card.Color.Diamond)));
    print_endline ( string_of_int (Card.compare (Card.newCard Card.Value.As Card.Color.Spade) (Card.newCard Card.Value.King Card.Color.Diamond)));
    print_endline "-------------";
    let rec printall l = match l with
        | []            -> ()
        | head :: tail  -> print_endline (Card.toStringVerbose head); printall tail
    in
    printall Card.all;
    print_endline "-------------";
    let rec printall l = match l with
        | []            -> ()
        | head :: tail  -> print_endline (Card.toString head); printall tail
    in
    printall Card.allHearts;
    print_endline "-------------";
    print_endline ( Card.toStringVerbose (Card.best [(Card.newCard Card.Value.King Card.Color.Spade); (Card.newCard Card.Value.As Card.Color.Diamond)]));
    print_endline ( Card.toStringVerbose (Card.best [(Card.newCard Card.Value.King Card.Color.Spade); (Card.newCard Card.Value.As Card.Color.Diamond) ;
                                                     (Card.newCard Card.Value.King Card.Color.Diamond)]));
    print_endline ( Card.toStringVerbose (Card.best [(Card.newCard Card.Value.As Card.Color.Spade); (Card.newCard Card.Value.As Card.Color.Diamond)]));
    print_endline ( Card.toStringVerbose (Card.best []))
