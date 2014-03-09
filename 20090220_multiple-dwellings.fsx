open Microsoft.FSharp.Reflection

module Array =
    /// <summary>
    /// Yields all permutations of a given array.
    /// </summary>
    /// <param name="array">The elements whose order shall be permuted.</param>
    /// <remarks>Do not change the array during the iteration!</remarks>
    let permutations array =
        let n = Array.length array
        let swap i j =
            let temp = array.[i]
            array.[i] <- array.[j]
            array.[j] <- temp
        let rec loop l =
            seq {
                if l + 1 >= n then
                    yield array
                else
                    yield! loop (l + 1)
                    for i = l + 1 to n - 1 do
                        swap l i
                        yield! loop (l + 1)
                        swap l i
            }
        loop 0

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
    Array.permutations ns
        |> Seq.find valid

moveIn()
    |> printfn "%A"

