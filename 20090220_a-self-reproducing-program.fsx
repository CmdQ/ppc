open System
let replace (needle:char) (haystack:string) straw =
    let index = haystack.IndexOf(needle)
    haystack.Remove(index, 1).Insert(index, straw)
let threeQuotes = String(Array.init 3 (fun _ -> '"'))
let prog = """open System
let replace (needle:char) (haystack:string) straw =
    let index = haystack.IndexOf(needle)
    haystack.Remove(index, 1).Insert(index, straw)
let threeQuotes = String(Array.init 3 (fun _ -> '"'))
let prog = $
replace '$' prog (threeQuotes + prog + threeQuotes) |> printf "%s"
"""
replace '$' prog (threeQuotes + prog + threeQuotes) |> printf "%s"
