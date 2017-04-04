(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ackermann.ml                                       :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: jschotte <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2017/04/04 08:59:16 by jschotte          #+#    #+#             *)
(*   Updated: 2017/04/04 09:23:57 by jschotte         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let rec ackermann m n =
    if m < 0 || n < 0
    then
        -1
    else if m = 0
    then
        n + 1
    else if m > 0 && n = 0
    then
        ackermann (m - 1) 1
    else
        ackermann (m-1) (ackermann m (n-1))

let () =
    print_int (ackermann (-1) 7);
    print_char '\n';
    print_int (ackermann 0 0);
    print_char '\n';
    print_int (ackermann 2 3);
    print_char '\n';
    print_int (ackermann 4 1);
    print_char '\n'
