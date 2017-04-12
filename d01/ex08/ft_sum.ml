(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_sum.ml                                          :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: jschotte <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2017/04/04 11:27:12 by jschotte          #+#    #+#             *)
(*   Updated: 2017/04/04 11:31:10 by jschotte         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)


let ft_sum f i j =
    let rec add f i j sum =
        if i > j
        then sum
        else add f (i + 1) j (sum +. f i)
    in
    add f i j 0.0

let () = 
    print_float (ft_sum (fun i -> float_of_int (i * i)) 1 10);
    print_char '\n'
