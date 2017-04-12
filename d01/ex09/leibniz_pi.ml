(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   leibniz_pi.ml                                      :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: jschotte <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2017/04/04 11:31:33 by jschotte          #+#    #+#             *)
(*   Updated: 2017/04/05 20:01:52 by jschotte         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let leibniz_pi delta =
    let ref = 4. *. (atan 1.) in
    let rec loop ref x value =
        if delta < 0.0
        then -1
        else if value <= ref +. delta && value >= ref -. delta
        then x
        else loop ref (x + 1) (value +. (4. *. ((-1.) ** float_of_int x) /. (2. *. (float_of_int x) +. 1.)))
    in 
    loop ref 0 0.0

let () = 
    print_int (leibniz_pi 0.1);
    print_char '\n';
    print_int (leibniz_pi 0.01);
    print_char '\n';
    print_int (leibniz_pi 0.005);
    print_char '\n';
    print_int (leibniz_pi 0.001);
    print_char '\n';
    print_int (leibniz_pi 0.0005);
    print_char '\n';
    print_int (leibniz_pi 0.0001);
    print_char '\n';
    print_int (leibniz_pi 0.000001);
    print_char '\n'
