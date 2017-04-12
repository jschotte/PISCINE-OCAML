(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_print_comb.ml                                   :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: jschotte <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2017/04/03 11:09:03 by jschotte          #+#    #+#             *)
(*   Updated: 2017/04/03 18:36:30 by jschotte         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let ft_print_nb x y =
    if x < 10
    then print_int 0;
    print_int x;
    print_char ' ';
    if (y < 10)
    then print_int 0;
    print_int y;
    if x <> 98 || y <> 99
    then
        print_string ", " 

let ft_print_comb2 () =
    let rec loop x y =
        if x = 99 && y = 99
        then ()
        else if x < y
        then
            ft_print_nb x y;
       if y < 99
       then 
           loop x (y + 1)
       else if x < 99
       then 
           loop (x + 1) 0
    in
    loop 0 0;
    print_char '\n'

let () = 
    ft_print_comb2 ()
