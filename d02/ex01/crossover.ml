(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   crossover.ml                                       :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: jschotte <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2017/04/05 14:08:53 by jschotte          #+#    #+#             *)
(*   Updated: 2017/04/05 20:04:11 by jschotte         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let rec is_in_list l x = match l with
    | [] -> false
    | h::t -> if h = x
              then true
              else is_in_list t x

let rec crossover l1 l2 = match l1 with
    | [] -> []
    | h :: t -> if (is_in_list l2 h) = true
              then h :: crossover t l2
              else crossover t l2


let print_list f lst =
      let rec print_elements = function
          | [] -> ()
          | h::t -> f h; print_string ";"; print_elements t
      in
      print_string "[";
      print_elements lst;
      print_string "]";;


let () =
    let l1 = ["1"; "2"; "3"; "42"]
    and l2 = ["42"]
    in
    print_list print_string (crossover l1 l2)
