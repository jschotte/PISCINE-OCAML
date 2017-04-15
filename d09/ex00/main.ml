let () =
    for i = -12 to 12 do
        let watch1:Watchtower.hour = (i:Watchtower.hour) in
        let watch2:Watchtower.hour = 2 in
        let sum:Watchtower.hour = Watchtower.add watch1 watch2 in
        let sub:Watchtower.hour = Watchtower.sub watch1 watch2 in
        Printf.printf "%d + %d = %d\n" watch1 watch2 sum; 
        Printf.printf "%d - %d = %d\n" watch1 watch2 sub; 
    done
    
