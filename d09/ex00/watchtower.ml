type hour = int
let zero = 12
    
let add x y =
    let result = (x + y) mod 12 in 
    if (result = 0) then zero
    else if result < 0 then zero + result
    else result
    
let sub x y =
    let res = (x - y) mod 12 in
    if res = 0 then zero
    else if res < 0 then zero - (res*(-1)) mod 12
    else res

