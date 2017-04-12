let () =
    Random.self_init();
    let jokes = [|"Quel super héros donne le plus vite l'heure ? -  Speed heure man (spiderman) !";
                  "Ce matin, j'ai voulu faire une blague sur le Super U, mais elle n'a pas Supermarché !";
                  "Quel est le comble pour un professeur de musique ? - Mettre des mauvaises notes";
                  "Qu'est ce qui pleure quand ont lui tourne la tête ? - Un robinet";
                  "J'ai cru que Mozart était mort mais mozzarella"|] 
    in
    print_endline jokes.(Random.int (Array.length jokes))
