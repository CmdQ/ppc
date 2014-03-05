open System

let fmod n d =
    n / d, n % d

let spigot n =
    let plus1 = (+)1uy >> (fun x -> x % 10uy)
    let m = float n * 10.0 / 3.0 |> ceil |> int
    let table = ref <| Array.init m (fun _ -> 2)
    let rec loop n (pre:byte list) (acc:byte list) =
        let ret = loop -1 []
        if n < 0 then
            List.tail acc |> List.rev
        elif n = 0 then
            let postprocessed =
                match pre with
                | 9uy::_ ->
                    List.map plus1 pre @ acc
                | l::rest ->
                    (plus1 l)::rest @ acc
                | _ ->
                    pre@acc
            ret postprocessed
        else
            let table = !table
            for i = Array.length table - 1 downto 0 do
                table.[i] <- table.[i] * 10
            for k = m - 1 downto 1 do
                let reduce = 2 * k + 1
                let q, r = fmod table.[k] reduce
                table.[k] <- r
                table.[k - 1] <- table.[k - 1] + q * k
                assert(k > 1 || q <= 19)
            let q, r = fmod table.[0] 10
            table.[0] <- r
            let q = q |> byte

            let n = n - 1
            if q = 9uy then
                loop n (q::pre) acc
            elif q = 10uy then
                loop n [0uy] (List.map plus1 pre @ acc)
            else
                loop n [q] (pre@acc)
    loop (n + 1) [] []

#if COMPILED
[<EntryPoint>]
#endif
let main _ =
    let text = "3.141592653589793238462643383279";
    let check =
        spigot <| text.Length - 1
        |> List.map (fun c -> char c + '0')
        |> List.toArray
    let check = String(check).Insert(1, ".")
    assert(check = text)
    for i = 1 to 75 do
        printf "%2d: " i
        spigot i |> List.iter (printf "%d")
        printfn ""
    0

#if INTERACTIVE
main [||]
#endif