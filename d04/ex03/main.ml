let () =
    let rec displayDeck deck = match deck with
            | []            -> ()
            | head :: tail  -> print_string ((head) ^ " "); displayDeck tail
    in
    displayDeck (Deck.toStringList (Deck.newDeck ()));
    print_newline ();
    let deck = Deck.newDeck () in
    let rec displayDeck deck = match deck with
            | []            -> ()
            | head :: tail  -> print_string ((head) ^ " "); displayDeck tail
    in
    displayDeck (Deck.toStringListVerbose deck);
    print_newline ();
    let (n, c) as card = Deck.drawCard deck in
     print_string "Tirage: "; print_endline (Deck.Card.toStringVerbose n);
    let (n, c) as card = Deck.drawCard c in
     print_string "Tirage: "; print_endline (Deck.Card.toStringVerbose n)
