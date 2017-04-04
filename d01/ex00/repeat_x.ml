(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   repeat_x.ml                                        :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: jschotte <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2017/04/04 08:30:43 by jschotte          #+#    #+#             *)
(*   Updated: 2017/04/04 08:49:26 by jschotte         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let rec repeat_x x =
    if x > 0
    then
        "x" ^ (repeat_x (x-1))
    else if x < 0
    then
        "Error"
    else
        ""


let () =
    print_endline (repeat_x 0);
    print_endline (repeat_x 1);
    print_endline (repeat_x (-1));
    print_endline (repeat_x 5);
    print_endline (repeat_x 42)

