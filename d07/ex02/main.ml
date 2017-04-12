let () =
    let dalek = new Dalek.dalek in
    let doctor = new Doctor.doctor "doc" 12 in
    let people = new People.people "Personne" in
    doctor#talk;
    print_endline dalek#to_string;
    dalek#talk;
    dalek#exterminate people;
    dalek#talk;
    print_endline dalek#to_string;
    dalek#die
