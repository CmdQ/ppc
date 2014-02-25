open System

type Message =
    | Cross of int
    | Query of AsyncReplyChannel<int>

type Cell =
    | Free of int
    | Set

type Actor = MailboxProcessor<Message>
type Board = Cell [,]

let size = 5
   
let shuffle a =
    let rand = Random(Guid.NewGuid().GetHashCode())
    for i = Array.length a - 1 downto 0 do
        let n = rand.Next(i + 1)
        let temp = a.[i]
        a.[i] <- a.[n]
        a.[n] <- temp
    a

let randomNoDouble length lo hi =
   let all = [| lo..hi |] |> shuffle
   Array.sub all 0 length

let randomBoard () : Board =
    let r = Array.init size (fun i -> randomNoDouble size (i * 15) (i * 15 + 15))
    Array2D.init size size (fun x y ->
            match x, y with
            | 2, 2 -> Set
            | x, y -> Free r.[y].[x]
        )

let set (board:Board) num =
    let num = Free num
    for y = 0 to size - 1 do
        for x = 0 to size - 1 do
            if board.[x, y] = num then
                board.[x, y] <- Set

let won (board:Board) =
    let isTrue = ((=)true)
    let allTrue = Array.forall isTrue
    let isSet x y = board.[x, y] = Set
    let fiveBool = Array.init size
    let hor y = fiveBool (fun x -> isSet x y)
    let ver x = fiveBool (fun y -> isSet x y)
    let diag f = fiveBool (fun i -> isSet i (f i))
    let diag2 = diag (fun i -> size - 1 - i)
    let diag = diag id
    let w =
        seq {
            yield allTrue diag
            yield allTrue diag2
            for i = 0 to size - 1 do
                yield hor i |> allTrue
                yield ver i |> allTrue
        } |> Seq.tryFind isTrue
    match w with
    | None -> 1
    | _ -> 0


let sheet () =
    Actor.Start(fun inbox ->
        let board = randomBoard()
        let set = set board
        let rec loop n =
            async {
                let! msg = inbox.Receive()
                match msg with
                | Cross num ->
                    set num
                    return! loop (n + won board)
                | Query(channel) ->
                    assert(inbox.CurrentQueueLength = 0)
                    assert(won board = 0)
                    channel.Reply(n)
                    return! loop n
            }
        loop 0)

let game players =
    let draw = [| 1..75 |] |> shuffle
    let players = [for i = 1 to players do yield sheet()]
    for num in draw do
        for player in players do
            player.Post(Cross num)
    players
        |> List.map (fun a -> a.PostAndReply(fun c -> Query c))
        |> List.min

let rec loop delta sum n =
    let draws = game 500 |> float
    if n = 0.0 then loop delta draws 1.0 else
    let without = sum / n
    let sum, n = sum + draws, n + 1.0
    let withit = sum / n
    if abs(without - withit) < delta && n > 100.0 then withit else
    loop delta sum n

let main () = 
    loop 0.01 0.0 0.0 |> printfn "%.2f"
    0

main()