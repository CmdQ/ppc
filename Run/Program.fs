#if COMPILED
module Program
#endif

open System

/// <summary>
/// Returns the quotient and the remainder of an integer division as a tuple.
/// </summary>
/// <param name="n">The numerator.</param>
/// <param name="d">The denominator.</param>
let fmod n d = n / d, n % d

/// <summary>
/// Increments a digit wrapping around from 9 to 0.
/// </summary>
let plus1 = (+)1uy >> (fun x -> x % 10uy)

let spigot n =
    if n = 1 then [3uy] else
    let m = 1 + (float n * 10.0 / 3.0 |> floor |> int)
    let table = ref <| Array.init m (fun _ -> 2)
    let rec loop n (pre:byte list) (acc:byte list) =
        if n < 0 then
            List.tail acc |> List.rev
        elif n = 0 then
            let postprocessed =
                match pre with
                | [] -> acc
                | 9uy::_ ->
                    List.map plus1 pre @ acc
                | l::rest ->
                    (plus1 l)::rest @ acc
            loop -1 [] postprocessed
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
    let text = "3141592653589793238462643383279502884197169399375105820974944592307816406286208998628034825342117067982148086513282306647093844609550582231725359408128481117450284102701938521105559644622948954930381964428810975665933446128475648233786783165271201909145648566923460348610454326648213393607260249141273724587006606315588174881520920962829254091715364367892590360011330530548820466521384146951941511609433057270365759591953092186117381932611793105118548074462379962749567351885752724891227938183011949129833673362440656643086021394946395224737190702179860943702770539217176293176752384674818467669405132000568127145263560827785771342757789609173637178721468440901224953430146549585371050792279689258923542019956112129021960864034418159813629774771309960518707211349999998372978049951059731732816096318595024459455346908302642522308253344685035261931188171010003137838752886587533208381420617177669147303598253490428755468731159562863882353787593751957781857780532171226806613001927876611195909216420198938095257201065485863278865936153381"
    for i = 1 to text.Length do
        let check =
            spigot i
            |> List.toArray
            |> Array.map (fun c -> char c + '0')
        let check = String(check)
        assert(check = text.Substring(0, i))
    for i = 1 to 75 do
        printf "%2d: " i
        spigot i |> List.iter (printf "%d")
        printfn ""
    0

#if INTERACTIVE
main fsi.CommandLineArgs
#endif