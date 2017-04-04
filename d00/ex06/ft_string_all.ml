(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_string_all.ml                                   :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: jschotte <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2017/04/03 13:02:01 by jschotte          #+#    #+#             *)
(*   Updated: 2017/04/03 18:39:40 by jschotte         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let is_digit c = c >= '0' && c <= '9'
let is_alpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

let ft_string_all pred str =
    let l = (String.length str) - 1 in
    let rec loop i =
        if i > l
        then true
        else if pred (String.get str (i)) = false
        then false
        else
            loop (i + 1);
    in
    loop 0

let () = 
    if ft_string_all is_digit "1234" = true
    then
        print_endline "TRUE"
    else
        print_endline "FALSE";
    
    if ft_string_all is_alpha "abcd" = true
    then
        print_endline "TRUE"
    else
        print_endline "FALSE";
    
    if ft_string_all is_digit "12a" = true
    then
        print_endline "TRUE"
    else
        print_endline "FALSE";

    if ft_string_all is_digit "a" = true
    then
        print_endline "TRUE"
    else
        print_endline "FALSE";

    if ft_string_all is_digit "12a4" = true
    then
        print_endline "TRUE"
    else
        print_endline "FALSE";

   if ft_string_all is_alpha "ab42d" = true
    then
        print_endline "TRUE"
    else
        print_endline "FALSE"
