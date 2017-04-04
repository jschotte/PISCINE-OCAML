(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   repeat_x.ml                                        :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: jschotte <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2017/04/04 08:30:43 by jschotte          #+#    #+#             *)
(*   Updated: 2017/04/04 08:59:07 by jschotte         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

   
let rec repeat_string ?str:(str="x") n = 
 if n > 0
    then
        str ^ (repeat_string ~str (n-1))
    else if n < 0
    then
        "Error"
    else
        ""


let () =
    print_endline (repeat_string (-1));
    print_endline (repeat_string 0);
    print_endline (repeat_string 1);
    print_endline (repeat_string ~str:"Toto" 1);
    print_endline (repeat_string 2);
    print_endline (repeat_string ~str:"a" 5);
    print_endline (repeat_string ~str:"what" 3)

