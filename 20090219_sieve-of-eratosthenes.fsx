(*
type Message =
    | Die
    | Next of AsyncReplyChannel<int>

type Actor = MailboxProcessor<Message>

let (<--) (mb:Actor) msg = mb.Post msg
let (<-->) (mb:Actor) f = mb.PostAndAsyncReply f

type Primes() =
    let counter () =
        Actor.Start(fun inbox ->
            let rec loop n step1 step2 =
                async {
                    let! msg = inbox.Receive()
                    match msg with
                    | Die -> return ()
                    | Next reply ->
                        reply.Reply n
                        if n <= 2 then
                            return! loop 3 2 4
                        elif n <= 3 then
                            return! loop 5 2 4
                        else
                            return! loop (n + step1) step2 step1
                }
            loop 3 0 0
        )

    let filter (c:Actor) pred =
        Actor.Start(fun inbox ->
            let rec loop () =
                async {
                    let! msg = inbox.Receive()
                    match msg with
                    | Die ->
                        c <-- Die
                        return ()
                    | Next reply ->
                        let rec filter n =
                            if pred n then async { return n } else
                            async {
                                let! m = c <--> Next
                                return! filter m
                            }
                        let! testItem = c <--> Next
                        let! filteredItem = filter testItem
                        reply.Reply filteredItem
                        return! loop()
                }
            loop()
        )

    let processor =
        Actor.Start(fun inbox ->
            let rec loop (oldFilter:Actor) prime =
                async {
                    let! msg = inbox.Receive()
                    match msg with
                    | Die ->
                        oldFilter <-- Die
                        return ()
                    | Next reply ->
                        reply.Reply prime
                        let newFilter = filter oldFilter (fun x -> x % prime <> 0)
                        let! newPrime = oldFilter <--> Next
                        return! loop newFilter newPrime
                }
            loop (counter ()) 2
        )

    member this.Next() = processor.PostAndReply((fun reply -> Next reply), 2000)
        
    interface System.IDisposable with
        member this.Dispose () = processor <-- Die

    static member UpTo max =
        use p = new Primes()
        Seq.initInfinite (fun _ -> p.Next())
        |> Seq.takeWhile ((>) max)
        |> Seq.toList
*)

let eratosthenes n =
    let n = n + 1
    let sq = n |> float |> sqrt |> int
    let nums = Array.init n (fun _ -> true)
    nums.[0] <- false
    nums.[1] <- false

    let rec crossout s i =
        if i < n then
            nums.[i] <- false
            crossout s (i + s)

    crossout 2 4
    
    let rec loop i =
        if i <= sq then
            let prime = array.IndexOf(nums, true, i)
            crossout (prime <<< 1) (prime * prime)
            loop (i + 1)
            
    loop 3
    nums

(*
let count = 9999
let era =
    eratosthenes count
    |> Array.mapi (fun i b -> if b then i else 0)
    |> Array.filter (fun p -> p > 0)
let mods = Primes.UpTo count
era = List.toArray mods
*)

let threshold = 15485863
eratosthenes threshold
    |> Seq.filter (fun b -> b)
    |> Seq.length
    |> printfn "Below %d there are %d primes." threshold