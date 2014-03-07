#if COMPILED
module Program
#else
#time;;
#r ".\packages\FSPowerPack.Core.Community.3.0.0.0\Lib\Net40\FSharp.PowerPack.dll"
#endif

open System
open Microsoft.FSharp.Collections

let inline (^) elm func = LazyList.consDelayed elm func

/// <summary>
/// Yields numbers that start with the square and advance in steps twice the original number.
/// </summary>
/// <param name="n">The number whose square to start with.</param>
/// <example>5 : 25, 35, 45, 55, ...</example>
let strike n =
    LazyList.unfold (fun (pos, step) ->
            if pos <= Int32.MaxValue - step then
                Some(pos, (pos + step, step))
            else
                None
        ) (pown n 2, 2 * n)

let last = ref (0, 0)

/// <summary>
/// Yields elements from np that are not part of cp.
/// </summary>
/// <param name="np">The ordered list to remove elements from.</param>
/// <param name="cp">The ordered list of elements to remove.</param>
let rec without np cp =
    match np, cp with
    | LazyList.Cons(p, npr), LazyList.Cons(c, cpr) ->
        if p < c then
            last := (p, c)
            p ^ (fun () -> without npr cp)
        elif p > c then
            last := (p, c)
            without np cpr
        else
            last := (p, c)
            without npr cpr
    | _ -> failwith "As both lists are theoretically infinite, we should never get here."

let inline (--) a b = without a b

let rec euler = function
    | LazyList.Cons(p, ps) ->
        let multiples = strike p
        p ^ (fun () -> ps -- multiples |> euler)
    | _ -> failwith "As the list is theoretically infinite, we should never get here."

let primes =
    /// <summary>
    /// Yields 5, 7, 11, 13, 15, 18...
    /// </summary>
    let around6 = LazyList.unfold (fun (a, b, c) -> Some(a, (a + b, c, b))) (5, 2, 4)
    LazyList.cons 2 (3 ^ (fun () -> euler around6))

#if COMPILED
[<EntryPoint>]
#endif
let main _ =
    let p = primes |> Seq.find ((<)100000)
    0

#if INTERACTIVE
main fsi.CommandLineArgs
#endif