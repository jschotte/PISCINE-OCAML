type 'a ft_ref = { mutable cont : 'a }

let return a = { cont = a }

let get a = a.cont

let set a x = a.cont <- x

let bind a f = return(get (f a.cont))

let fct_test x =
    x * 2

let () =
    let x = return 21 in
    Printf.printf "%d\n" (get x);
    set x 42;
    Printf.printf "%d\n" (get x);
    let y = bind x (fun i -> return (i * i)) in
    Printf.printf "%d\n" (get y)
