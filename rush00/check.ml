        let verification t = match t with
        | a :: b :: c :: d :: e :: f :: g :: h :: i :: [] -> if a = b && a = X then 3
                                                             else if a = c && c = X then 2
                                                             else if b = c && c = X then 1
                                                             else if d = e && e = X then 6
                                                             else if d = f && f = X then 5
                                                             else if e = f && f = X then 4
                                                             else if g = h && h = X then 7
                                                             else if g = i && i = X then 8
                                                             else if h = i && a = X then 9

                                                             else if a = d && d = X then 7
                                                             else if a = g && g = X then 4
                                                             else if d = g && d = X then 1
                                                             else if b = e && e = X then 8
                                                             else if b = h && h = X then 5
                                                             else if h = e && e = X then 2

                                                             else if c = f && f = X then 9
                                                             else if c = i && i = X then 6
                                                             else if i = f && f = X then 3
                                                                    
                                                             else if a = e && e = X then 9
                                                             else if a = i && i = X then 5
                                                             else if i = e && e = X then 1

                                                             else if c = e && e = X then 7
                                                             else if c = g && g = X then 5
                                                             else if g = e && e = X then 3
    (*    NUll NATCH      *)                                 else (-1)
       | _                                                 -> 10

