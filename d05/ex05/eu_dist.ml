let sum x y =
        x +. y

let eu_dist x y =
    let start = 0. in
    let result = ref start in
    for i=0 to (Array.length x) -1 do
        result := (!result +. ((x.(i) -. y.(i)) *. (x.(i) -. y.(i))));
    done;
    sqrt !result

let () =
    let x = [|4.2;0.42;42.|] in 
    let y = [|2.1;0.21;21.|] in 
    print_float (eu_dist x y)
        
