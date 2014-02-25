open System
open System.Text.RegularExpressions

let eval elms =
    let rec eval elms (stack:float list) = 
        match elms, stack with
        | [], [only] -> only
        | "+"::es, f::s::rest ->
            eval es ((s + f)::rest)
        | "-"::es, f::s::rest ->
            eval es ((s - f)::rest)
        | "*"::es, f::s::rest ->
            eval es ((s * f)::rest)
        | "/"::es, f::s::rest ->
            eval es ((s / f)::rest)
        | e::es, stack ->
            match Double.TryParse(e, Globalization.NumberStyles.Float, Globalization.CultureInfo.InvariantCulture) with
            | true, f ->
                eval es (f::stack)
            | _ -> failwith "Unparsable float."
        | _ -> failwith "Invalid input."
    eval (Array.toList elms) []

let rec repl line =
    if line = null then
        Console.ReadLine() |> repl
    elif String.IsNullOrWhiteSpace(line) |> not then
        let elms = Regex.Split(line, @"\s+")
        eval elms |> printfn "%f"
        repl null

assert (abs(eval [| "19"; "2.14"; "+"; "4.5"; "2"; "4.3"; "/"; "-"; "*" |] - 85.2974) < 0.0001)
        
repl null
