(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_print_comb.ml                                   :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: jschotte <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2017/04/03 11:09:03 by jschotte          #+#    #+#             *)
(*   Updated: 2017/04/03 18:18:41 by jschotte         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let ft_print_nb x y z=
    print_int x;
    print_int y;
    print_int z;
    if x <> 7 || y <> 8 || z <> 9
    then
        print_string ", " 

let ft_print_comb () =
    let rec loop x y z =
        if x = 9 && y = 9 && z = 9
        then ()
        else if x < y && y < z 
        then
            ft_print_nb x y z;
       if z < 9
       then 
           loop x y (z + 1)
       else if y < 8
       then 
           loop x (y + 1) (y + 2)
       else if x < 7
       then 
           loop (x + 1) (x + 2) (x + 3)
    in
    loop 0 0 0;
    print_char '\n'

let () = 
    ft_print_comb ()
