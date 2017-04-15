type project = string * string * int
    
let zero = (("", "", 0):project)
        
let combine ((s1, t1, x1):project) ((s2, t2, x2):project) =
    let res = (x1+x2)/2 in
    if res >= 80
    then ((s1 ^ s2, "succeed", res):project)
    else ((s1 ^ s2, "failed", res):project)
        
let fail ((s, t, x):project) = ((s, "failed", 0):project)
let sucess ((s, t, x):project) = ((s, "succeed", 80):project)
