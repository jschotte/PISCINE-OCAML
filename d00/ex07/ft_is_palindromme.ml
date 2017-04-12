(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_is_palindromme.ml                               :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: jschotte <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2017/04/03 14:42:19 by jschotte          #+#    #+#             *)
(*   Updated: 2017/04/03 18:39:53 by jschotte         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let ft_is_palindrome str =
    let l = (String.length str) - 1 in
    let rec loop i =
        if i > l
        then true
        else if (String.get str i) <> (String.get str (l-i))
        then false
        else loop(i + 1);
    in 
    loop 0

let () =
      if ft_is_palindrome "radar" = true
    then
        print_endline "TRUE"
    else
        print_endline "FALSE";
    
    if ft_is_palindrome "madam" = true
    then
        print_endline "TRUE"
    else
        print_endline "FALSE";
    
    if ft_is_palindrome "car" = true
    then
        print_endline "TRUE"
    else
        print_endline "FALSE";

    if ft_is_palindrome "1231" = true
    then
        print_endline "TRUE"
    else
        print_endline "FALSE";

    if ft_is_palindrome "1221" = true
    then
        print_endline "TRUE"
    else
        print_endline "FALSE";

    if ft_is_palindrome "" = true
    then
        print_endline "TRUE"
    else
        print_endline "FALSE"
