open Microsoft.FSharp.Reflection

type Name =
    | Baker
    | Cooper
    | Fletcher
    | Miller
    | Smith

let valid = function
    | [| bottom; first; second; third; top |] as floors ->
        let pos n =
            let i = array.IndexOf(floors, n)
            if i < 0 then failwith "Name not in array."
            i
        let adjacent p q =
            abs(pos p - (pos q)) = 1
        top <> Baker
            && bottom <> Cooper
            && top <> Fletcher && bottom <> Fletcher
            && pos Miller > pos Cooper
            && not (adjacent Smith Fletcher)
            && not (adjacent Cooper Fletcher)
    | _ -> invalidArg "" "Expecting an array of 5 names."

let moveIn () =
    let ns =
        FSharpType.GetUnionCases(typeof<Name>)
            |> Array.map (fun uci -> FSharpValue.MakeUnion(uci, null) :?> Name)
    let n = Array.length ns
    let swap i j =
        let temp = ns.[i]
        ns.[i] <- ns.[j]
        ns.[j] <- temp
    let rec enum l =
        seq {
            if l + 1 >= n then
                yield ns
            else
                yield! enum (l + 1)
                for i = l + 1 to n - 1 do
                    swap l i
                    yield! enum (l + 1)
                    swap l i
        }
    enum 0
        |> Seq.find valid

moveIn()
    |> printfn "%A"

