let () =
    let doctor = new Doctor.doctor "Doctor" 42 in
    print_endline doctor#to_string;
    doctor#talk;
    doctor#use_sonic_screwdriver;
    doctor#travel_in_time 1950 2016;
    print_endline doctor#to_string
