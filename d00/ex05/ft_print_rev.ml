(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_print_rev.ml                                    :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: jschotte <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2017/04/03 12:43:55 by jschotte          #+#    #+#             *)
(*   Updated: 2017/04/03 12:58:54 by jschotte         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let ft_print_rev str =
    let x = (String.length str) - 1 in
    let rec loop i = 
        if i <= x
        then
            begin
                print_char (String.get str (x - i));
                loop (i + 1)
            end
    in
    loop 0;
    print_char '\n'
 

let () =
    ft_print_rev "TEST";
    ft_print_rev "Hello World !";
    ft_print_rev "";
    ft_print_rev " ";
    ft_print_rev "42";
    ft_print_rev "a"
