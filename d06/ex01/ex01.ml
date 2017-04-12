module HashString =
    struct
        type t = string
        let equal a b = a = b
        let hash s =
            let sum = ref 0 in
            String.iter (fun x -> sum := !sum + (Char.code x)) s;
            !sum
    end

module StringHashtbl = Hashtbl.Make(HashString)

let () =
    let ht = StringHashtbl.create 5 in
    let values = [ "Hello"; "world"; "42"; "Ocaml"; "H" ] in
    let pairs = List.map (fun s -> (s, String.length s)) values in
    List.iter (fun (k,v) -> StringHashtbl.add ht k v) pairs;
    StringHashtbl.iter (fun k v -> Printf.printf "k = \"%s\", v = %d\n" k v) ht
