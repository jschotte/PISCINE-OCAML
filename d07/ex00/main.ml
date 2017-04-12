let () =
    let test = new People.people "Jeremy" in
    print_endline test#to_string;
    test#talk;
    test#die
