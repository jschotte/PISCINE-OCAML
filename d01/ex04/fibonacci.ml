(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   fibonacci.ml                                       :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: jschotte <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2017/04/04 09:28:39 by jschotte          #+#    #+#             *)
(*   Updated: 2017/04/05 19:53:58 by jschotte         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let fibonacci n =
    let rec fibo n acc b =
        if n < 0
        then -1
        else if n < 1
        then acc
        else fibo (n - 1) b (acc + b)
    in
    fibo n 0 1

let () =
    print_int (fibonacci (-42));
    print_char '\n';
    print_int (fibonacci 0);
    print_char '\n';
    print_int (fibonacci 1);
    print_char '\n';
    print_int (fibonacci 2);
    print_char '\n';
    print_int (fibonacci 3);
    print_char '\n';
    print_int (fibonacci 4);
    print_char '\n';
    print_int (fibonacci 5);
    print_char '\n';
    print_int (fibonacci 6);
    print_char '\n';
    print_int (fibonacci 7);
    print_char '\n';
    print_int (fibonacci 8);
    print_char '\n'
