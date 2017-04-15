type 'a t = Success of 'a | Failure of exn

let return a = Success a

exception Except

let bind m f = match m with
        | Success x     -> begin
                               try f x
                               with
                               | x  -> Failure x
                            end;
        | Failure x     -> Failure x
        
let recover m f = match m with
        | Failure _     -> f m
        | _             -> m

let filter m b = match m with
        | Success _     -> if (b m) = false
                           then Failure Except
                           else m
        | _             -> m

let flatten m = match m with
        | Success x ->  begin
                                match x with
                                | Success y      -> x
                                | Failure y      -> Failure y
                            end
        | Failure x -> Failure x
    
let to_string = function
        | Success s     -> "Success : " ^ s
        | Failure _     -> "Failure"

