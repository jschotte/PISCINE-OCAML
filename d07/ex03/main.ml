let () =
    let dalek = new Dalek.dalek in
    let doctor = new Doctor.doctor "Doc" 12 in
    let army1 = new Army.army [ ] in
    let army2 = new Army.army [ ] in
    army1#add doctor;
    (*army1#add dalek;   --> Error because not same class*)
    army2#add dalek;
    army1#add doctor;
    army1#add doctor;
    army1#delete;
    army1#delete;
    army2#delete
