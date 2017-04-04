(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   hofstadter_mf.ml                                   :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: jschotte <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2017/04/04 09:58:07 by jschotte          #+#    #+#             *)
(*   Updated: 2017/04/04 10:06:06 by jschotte         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let rec hfs_f n =
    if n < 0
    then -1
    else if n = 0
    then 1
    else n - (hfs_m (hfs_f (n-1)))

and hfs_m n =
    if n < 0
    then -1
    else if n = 0
    then 0
    else n - (hfs_f (hfs_m (n-1)))

let () = 
    print_int (hfs_m 0);
    print_char '\n';
    print_int (hfs_f 0);
    print_char '\n';
    print_int (hfs_m 4);
    print_char '\n';
    print_int (hfs_f 4);
    print_char '\n'
