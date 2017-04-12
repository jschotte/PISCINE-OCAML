(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_test_sign.ml                                    :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: jschotte <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2017/04/03 09:46:19 by jschotte          #+#    #+#             *)
(*   Updated: 2017/04/03 10:02:50 by jschotte         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let ft_test_sign x =
    if x < 0 then
        print_endline "negative"
    else 
        print_endline "positive"

let () =
    ft_test_sign 42;
    ft_test_sign 0;
    ft_test_sign (-42)
