type 'a tree = Nil | Node of 'a * 'a tree * 'a tree

let draw_square x y size =
    let mid = size / 2 in
    Graphics.moveto (x + mid) (y + mid);
    Graphics.lineto (x - mid) (y + mid);
    Graphics.lineto (x - mid) (y - mid);
    Graphics.lineto (x + mid) (y - mid);
    Graphics.lineto (x + mid) (y + mid)

let draw_tree_node tree =
    let print_node x y str =
        draw_square x y 50;
        Graphics.moveto x y;
        Graphics.draw_string str
    in 
    match tree with 
        | Node (v, Nil, Nil) -> print_node 150 150 v;
                                Graphics.moveto 175 150;
                                Graphics.lineto 225 250;
                                print_node 250 250 "Nil";
                                Graphics.moveto 175 150;
                                Graphics.lineto 225 50;
                                print_node 250 50 "Nil"
        | _ -> ()


let main () =
    Graphics.open_graph (" 800 600 ");
    draw_tree_node (Node ("42", Nil, Nil));
    Graphics.read_key ()


let () =
    ignore(main ())
