module Watchtover =
    struct
        type hour = int
        let zero = 12
        let add x y =
            let res = (x + y) mod 12
            in if (res <= 0) then zero else res
        let sub x y =
            let res = (x - y) mod 12 in
            if res = 0 then zero
            else if res < 0 then (res * -1)
            else res
    end

let () =
    let watch1:Watchtover.hour = 12 in
    let watch2:Watchtover.hour = 1 in
    let sum:Watchtover.hour = Watchtover.add watch1 watch2 in
    let sub:Watchtover.hour = Watchtover.sub watch1 watch2 in
    let sub2:Watchtover.hour = Watchtover.sub watch2 watch1 in
    print_endline (string_of_int sum);
    print_endline (string_of_int sub);
    print_endline (string_of_int sub2)
