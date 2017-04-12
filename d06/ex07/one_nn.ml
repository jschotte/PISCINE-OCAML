let sum x y =
        x +. y

let eu_dist x y =
    let start = 0. in
    let result = ref start in
    for i=0 to (Array.length x) -1 do
        result := (!result +. ((x.(i) -. y.(i)) *. (x.(i) -. y.(i))));
    done;
    sqrt !result

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

type radar = (float array) * string

let find_one (radarlst: radar list) (rad: radar) =
    let  (tab, le) = rad in
    let rec getmin min letter l = match l with
        | []                -> letter
        | (x, y) :: tail    -> if  min = (-1.) || (eu_dist x tab) < min
                               then getmin (eu_dist x tab) y tail
                               else getmin min letter tail
    in
    getmin (-1.) "" radarlst

let () =
      let t = (example_of_file "ionosphere.train.csv") in
      print_string (find_one t ([|1.; 0.; 0.76627; 0.21106; 0.63935; 0.38112; 0.48409; 0.525; 0.15; 0.22273; 0.13753; 0.59565; -0.07727; 0.44545; 0.; 0.48636; -0.27491; 0.42014; -0.56136; 0.36818; -0.36591; 0.18864; -0.40533; 0.07588; -0.38483; -0.03229; -0.33942; -0.12486; -0.2754; -0.19714; -0.19962; -0.24648; -0.11894; -0.27218; |], "g"))
