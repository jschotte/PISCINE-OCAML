(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_rot_n.ml                                        :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: jschotte <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2017/04/03 15:16:33 by jschotte          #+#    #+#             *)
(*   Updated: 2017/04/03 17:03:47 by jschotte         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let is_upper c = (c >= 'A' && c <= 'Z')
let is_lower c = (c >= 'a' && c <= 'z')

let ft_rot_n n str =
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

let () =
    print_endline (ft_rot_n 1 "abcdefghijklmnopqrstuvwxyz");
    print_endline (ft_rot_n 13 "abcdefghijklmnopqrstuvwxyz");
    print_endline (ft_rot_n 42 "abcdefghijklmnopqrstuvwxyz");
    print_endline (ft_rot_n 16 "abcdefghijklmnopqrstuvwxyz");
    print_endline (ft_rot_n 0 "abcdefghijklmnopqrstuvwxyz");
    print_endline (ft_rot_n 2 "012345678");
    print_endline (ft_rot_n 0 "Damned !");
    print_endline (ft_rot_n 42 "");
    print_endline (ft_rot_n 1 "NBzlk qnbjr !")


