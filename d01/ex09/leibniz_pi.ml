(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   leibniz_pi.ml                                      :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: jschotte <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2017/04/04 11:31:33 by jschotte          #+#    #+#             *)
(*   Updated: 2017/04/04 14:38:15 by jschotte         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let ft_sum f i j =
    let rec add f i j sum =
        if i > j
        then sum
        else add f (i + 1) j (sum +. f i)
    in
    add f i j 0.0

let leibniz_pi delta =
    let ref = 4. *. (atan 1.) in
    let rec loop delta ref x value =
        if delta < 0.0
        then -1
        else if value < ref +. delta && value > ref -. delta
        then x
        else loop delta ref (x + 1) (4. *. ft_sum (fun i -> ((-1.) ** float_of_int i) /. (2. *. (float_of_int i) +. 1.)) 0 x)
    in 
    loop delta ref 1 4.0

let () = 
    print_int (leibniz_pi 0.1);
    print_char '\n';
    print_int (leibniz_pi 0.01);
    print_char '\n';
    print_int (leibniz_pi 0.001);
    print_char '\n';
    print_int (leibniz_pi 0.005);
    print_char '\n';
    print_int (leibniz_pi 0.0001);
    print_char '\n'
