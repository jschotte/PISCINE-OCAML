(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_power.ml                                        :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: jschotte <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2017/04/03 10:21:02 by jschotte          #+#    #+#             *)
(*   Updated: 2017/04/03 10:57:54 by jschotte         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let rec ft_power x y =
    if y == 0
        then 1
   else if x == 0
        then 0
    else if x <= 1
        then y
    else 
        y * ft_power (x-1) y

let () = 
    print_int (ft_power 2 4);
    print_char '\n';
    print_int (ft_power 3 3);
    print_char '\n';
    print_int (ft_power 3 0);
    print_char '\n';
    print_int (ft_power 0 5)

