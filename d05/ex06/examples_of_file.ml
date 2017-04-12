let create_tab lst =
    let myarray = Array.make (List.length lst - 1) 0.0 in
    for i=0 to (List.length lst - 2) do
       myarray.(i) <- float_of_string (List.nth lst i)
    done;
    myarray
    

let example_of_file path =
    let tup = [] in
  let tmp = ref tup in
  begin try
        let lc = open_in path in
        begin try
            while true; do
                let s = input_line lc in
                let split = String.split_on_char ',' s in
                let tab = create_tab (split) in
                let leter = List.nth split ((List.length split) - 1) in
                tmp := !tmp @ [(tab, leter)];
            done
        with
        | Sys_error err -> print_endline "Error"
        | End_of_file   -> ()
        end;
        close_in lc
        with
        | _     -> print_endline "Error to open file"
  end;
  !tmp

let print_tab tab =
    print_string "([|";
    for i=0 to ((Array.length tab) - 1) do
        print_float (tab.(i)); print_string "; ";
    done;
    print_string "|], "


let () =
    let rec loop lst = match lst with
            | []            -> ()
            | (x, y) :: tail  -> print_tab x;  print_string y; print_endline ")";  loop tail
    in
    loop (example_of_file "ionosphere.train.csv")



