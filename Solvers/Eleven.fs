module Eleven

let parse (lines: string seq) =
    lines |> Seq.head |> _.Split(" ") |> Array.toSeq |> Seq.map int64

let digits (x: int64) : int =
    if x < 10L then
        1
    else
        x |> float |> System.Math.Log10 |> int |> (+) 1

let evenDigits x = digits x % 2 = 0

let halve x =
    let dig = digits x
    let strx = string x

    seq {
        int64 strx[0 .. dig / 2 - 1]
        int64 strx[dig / 2 .. dig - 1]
    }

let blink =
    function
    | 0L -> Seq.singleton 1L
    | x when evenDigits x -> halve x
    | x -> x * 2024L |> Seq.singleton

let blinkMapping (x, n) = blink x |> Seq.map (fun y -> (y, n))

let sumMappings (key, s) =
    let sum = Seq.map snd s |> Seq.sum
    (key, sum)

let a stones =
    let folder state _ = Seq.collect blink state
    [ 1..25 ] |> List.fold folder stones |> Seq.length |> Some

let b stones =
    let map = stones |> Seq.map (fun x -> (x, 1L)) |> Map.ofSeq

    let folder state i =
        printfn "%d %A" i state

        state
        |> Map.toSeq
        |> Seq.collect blinkMapping
        |> Seq.groupBy fst
        |> Seq.map sumMappings
        |> Map.ofSeq

    [ 1..75 ] |> List.fold folder map |> Map.toSeq |> Seq.map snd |> Seq.sum |> Some

let solve input =
    let parsed = parse input
    let ansA = a parsed
    let ansB = b parsed
    (ansA, ansB)
