type t = X | O | N

let toString t = match t with
    | X     -> "X"
    | O     -> "O"
    | N     -> "-"

let isFree t = if t = N
               then true
               else false

let verification t = match t with
| a :: b :: c :: d :: e :: f :: g :: h :: i :: [] -> if a = b && b = c && a <> N then a
(* Honrizontale Check *)                                 else if d = e && e = f && d <> N then d
                                                     else if g = h && h = i && g <> N then g
                                                     else if a = d && d = g && a <> N then a
(*   Vertical Check   *)                                 else if b = e && e = h && b <> N then b
                                                     else if c = f && f = i && c <> N then c
(*    Cross Check     *)                                 else if a = e && e = i && a <> N then a
                                                     else if c = e && e = g && c <> N then c
(*    NUll NATCH      *)                                 else N
| _                                                 -> N











