let flavius n m =
    let m = m - 1
    let rec loop i alive dead =
        let n = Array.length alive
        if n = 0 then
            List.rev dead
        elif i >= n then
            loop (i % n) alive dead
        else
            let shot = alive.[i]
            loop (i + m) (Array.filter ((<>)shot) alive) (shot::dead)
    loop m [| 0..n-1 |] []

flavius 41 3
    |> Seq.iter (printfn "%d")
