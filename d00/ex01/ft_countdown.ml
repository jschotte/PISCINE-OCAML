(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_countdown.ml                                    :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: jschotte <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2017/04/03 10:03:23 by jschotte          #+#    #+#             *)
(*   Updated: 2017/04/03 10:20:36 by jschotte         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let rec ft_countdown x = 
    if x <= 0 then
        begin
            print_int 0;
            print_char '\n'
        end
    else
        begin
            print_int x;
            print_char '\n';
            ft_countdown (x - 1)
        end

let () = 
    ft_countdown 0;
    ft_countdown (-1);
    ft_countdown 10
