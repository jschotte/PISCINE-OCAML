(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_print_alphabet.ml                               :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: jschotte <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2017/04/03 10:59:34 by jschotte          #+#    #+#             *)
(*   Updated: 2017/04/03 17:05:43 by jschotte         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let ft_print_alphabet () = 
    let ascii_of_a = int_of_char 'a' in
    let ascii_of_z = int_of_char 'z' in
    let rec loop ascii_of_current_letter =
        if ascii_of_current_letter <= ascii_of_z
        then
            begin
                print_char (char_of_int ascii_of_current_letter);
                loop (ascii_of_current_letter + 1)
             end 
    in
    loop ascii_of_a;
    print_char '\n'

let () =
    ft_print_alphabet ()
