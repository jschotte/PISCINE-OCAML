let is_upper c = (c >= 'A' && c <= 'Z')
let is_lower c = (c >= 'a' && c <= 'z')

let caesar n str=
    let rot c =
        if is_lower c
        then
            char_of_int(((int_of_char(c) - int_of_char('a') + n) mod 26) + int_of_char('a'))
        else if is_upper c
        then
            char_of_int(((int_of_char(c) - int_of_char('A') + n) mod 26) + int_of_char('A'))
        else
            c;
    in
    String.map rot str

let rot42 str =
    caesar 42 str

let xor key str=
    let fxor c = char_of_int ((int_of_char c) lxor key)
    in String.map fxor str

let rec ft_crypt str f = match f with
    | [] -> str
    | h::t -> ft_crypt(h str) t

let () =
    print_endline (caesar 13 "abcde");
    print_endline (Uncipher.uncaesar 13 (caesar 13 "bonjour"));
    print_endline (xor 24 "abcde");
    print_endline (xor 24 (xor 24 "Lorem Ispum 42" ));
    print_endline (ft_crypt "abcde" [rot42; Uncipher.unrot42]);
    print_endline (Uncipher.ft_uncrypt (ft_crypt "abcde" [(xor 10); rot42]) [Uncipher.unrot42; (xor 10)]);
    print_endline (Uncipher.ft_uncrypt (ft_crypt "Test plus dur" [(xor 10); rot42; (caesar 12); rot42]) [Uncipher.unrot42; (Uncipher.uncaesar 12); Uncipher.unrot42; (xor 10)])
