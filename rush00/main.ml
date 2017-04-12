let is_digit c = c >= '0' && c <= '9'

let ft_string_all pred str =
    let l = (String.length str) - 1 in
    let rec loop i =
        if i > l
        then true
        else if pred (String.get str (i)) = false && String.get str (i) <> ' '
        then false
        else
        loop (i + 1);
    in
    loop 0

let () =
    let rec newGame () =
        let v = Value.O in
        print_endline "Entrez le nom du joueur 1";
        let name1 = read_line () in
        print_endline "Entrez le nom du joueur 2 (tapez IA pour jouer contre l'IA)";
        let name2 = read_line () in
        if name1 = name2
        then
            begin
                print_endline("Les 2 joueurs ne peuvent pas avoir le meme nom");
                newGame()
            end;
        let game = Game.newGame () in
        Game.printGame game;
        let rec whileGame game v oldx =
            if name2 = "IA" (*Begin IA*)
            then
                begin
                    if v = Value.O
                    then 
                        begin
                          if Game.check oldx game <> Value.N
                                || Map.verif_draw (List.nth game (oldx - 1)) = 0
                          then whileGame game Value.O (Game.getFirstMapFree game);
                          print_endline "Au tour de l'IA de jouer :";
                          let y = (Map.verificationIA (List.nth game (oldx - 1))) in
                           let ng = Game.replace_in_map oldx y v game in
                            if Game.check (oldx) ng = Value.O 
                                then print_endline ("Le joueur " ^ name2 ^ " (" ^ (Value.toString v) ^ ") a complete la grille " ^ string_of_int oldx);
                            Game.printGame ng;
                            let ng = Game.replace_in_map 10 oldx (Game.check oldx ng) ng in
                            Game.printEndMap ng;
                            if Game.check 10 ng = Value.O
                            then
                                begin
                                    print_endline ("L' IA (O) A GAGNE LA PARTIE");
                                    print_endline ("Appuyez sur 'Entree' pour rejouer !'");
                                    ignore (read_line ());
                                    newGame ()
                                end;
                            if Game.verif_draw ng = true
                            then
                                begin
                                    print_endline ("EGALITE");
                                    print_endline ("Appuyez sur 'Entree' pour rejoue !'");
                                    ignore (read_line ());
                                    newGame ()
                                end;
                            whileGame ng Value.X oldx;
                        end
                    else
                        begin
                            print_endline ("Au tour de " ^ name1 ^ " (X) de jouer, veuillez entrer les coordonnees ");
                            let input = read_line () in
                            if ft_string_all is_digit input = false         (* VERIF TYPE INPUT *)
                            then 
                                begin
                                    print_endline "Invalid params";
                                    whileGame game v 0;
                                end;
                            let split = String.split_on_char ' ' input in
                            if List.length split < 2 || (String.length (List.nth (split) 0) <= 0)
                                                     || (String.length (List.nth (split) 0) >= 2)
                                                     || (String.length (List.nth (split) 1) <= 0)
                                                     || (String.length (List.nth (split) 1) >= 2)
                            then 
                                begin
                                    print_endline "Invalid params";
                                    whileGame game v 0;
                                end;
                            let x = int_of_string (List.hd split) in
                            let y = int_of_string (List.nth split 1) in 
                            if Game.verif_input x y game = false             (* VERIF VALUE INPUT *)
                            then 
                                begin
                                    print_endline "Invalid params";
                                    whileGame game v x;
                                end;
                            let ng = Game.replace_in_map x y v game in
                            if Game.check x ng = Value.X
                            then 
                                print_endline ("Le joueur " ^ name1 ^ " (" ^ (Value.toString v) ^ ") a complete la grille " ^ string_of_int x);
                             Game.printGame ng;
                             let ng = Game.replace_in_map 10 x (Game.check x ng) ng in
                             Game.printEndMap ng;
                            if Game.check 10 ng = Value.X
                            then 
                                begin
                                    print_endline ("LE JOUEUR " ^ name1 ^ " (X) A GAGNE LA PARTIE");
                                    print_endline ("Appuyez sur 'Entree' pour rejoue !'");
                                    ignore (read_line ());
                                    newGame ()
                                end;
                            if Game.verif_draw ng = true
                            then
                                begin
                                    print_endline ("EGALITE");
                                    print_endline ("Appuyez sur 'Entree' pour rejoue !'");
                                    ignore (read_line ());
                                    newGame ()
                                end;
                            whileGame ng Value.O x;
                    end
                end                        (*END IA*)

            else                        
                begin
                    if v = Value.X
                    then print_endline ("Au tour de " ^ name1 ^ " (X) de jouer, veuillez entrer les coordonnees ")
                    else
                    print_endline ("Au tour de " ^ name2 ^ " (O) de jouer, veuillez entrer les coordonnees ");
                    let input = read_line () in
                    if ft_string_all is_digit input = false         (* VERIF TYPE INPUT *)
                    then 
                        begin
                            print_endline "Invalid params";
                            whileGame game v 0;
                        end;
                    let split = String.split_on_char ' ' input in
                    if List.length split < 2 || (String.length (List.nth (split) 0) <= 0)
                                             || (String.length (List.nth (split) 0) >= 2)
                                             || (String.length (List.nth (split) 1) <= 0)
                                             || (String.length (List.nth (split) 1) >= 2)
                    then 
                        begin
                            print_endline "Invalid params";
                            whileGame game v 0;
                        end;
                    let x = int_of_string (List.hd split) in
                    let y = int_of_string (List.nth split 1) in 
                    if Game.verif_input x y game = false             (* VERIF VALUE INPUT *)
                    then 
                        begin
                            print_endline "Invalid params";
                            whileGame game v 0;
                        end;
                    let ng = Game.replace_in_map x y v game in
                    if Game.check x ng = Value.X
                    then print_endline ("Le joueur " ^ name1 ^ " (" ^ (Value.toString v) ^ ") a complete la grille " ^ string_of_int x)
                    else if Game.check x ng = Value.O 
                        then print_endline ("Le joueur " ^ name2 ^ " (" ^ (Value.toString v) ^ ") a complete la grille " ^ string_of_int x);
                    Game.printGame ng;
                    let ng = Game.replace_in_map 10 x (Game.check x ng) ng in
                    Game.printEndMap ng;
                    if Game.check 10 ng = Value.O
                    then 
                        begin
                            print_endline ("LE JOUEUR " ^ name2 ^ " (O) A GAGNE LA PARTIE");
                            print_endline ("Appuyez sur 'Entree' pour rejouer !'");
                            ignore (read_line ());
                            newGame ()
                        end
                    else if Game.check 10 ng = Value.X
                    then 
                        begin
                            print_endline ("LE JOUEUR " ^ name1 ^ " (X) A GAGNE LA PARTIE");
                            print_endline ("Appuyez sur 'Entree' pour rejoue !'");
                            ignore (read_line ());
                            newGame ()
                        end;
                    if Game.verif_draw ng = true
                    then
                        begin
                            print_endline ("EGALITE");
                            print_endline ("Appuyez sur 'Entree' pour rejoue !'");
                            ignore (read_line ());
                            newGame ()
                        end;
                    if v = Value.X
                    then whileGame ng Value.O 0
                    else whileGame ng Value.X 0;
                end

        in
        whileGame game v 1
    in
    newGame ()
