(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   iter.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: jschotte <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2017/04/04 10:04:38 by jschotte          #+#    #+#             *)
(*   Updated: 2017/04/04 11:02:26 by jschotte         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let iter f x n =
    let rec loop f x n acc =
        if n < 0
         then -1
         else if n = 0
            then x
         else
             loop f (f x) (n-1) (acc + x)
    in
    loop f x n 0


let () =
    print_int (iter (fun x -> x * x) 2 4);
    print_char '\n';
    print_int (iter (fun x -> x * 2) 2 4);
    print_char '\n'
