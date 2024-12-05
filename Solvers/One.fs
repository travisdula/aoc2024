module One

let parse (lines: seq<string>) =
    lines |> Seq.map (fun x -> x.Split("   ") |> Seq.map int)

let a left right =
    Seq.zip (Seq.sort left) (Seq.sort right)
    |> Seq.map (fun (x, y) -> abs (x - y))
    |> Seq.sum

let b left right =
    let counts = Seq.countBy id right

    let ss leftItem =
        match Seq.tryFind (fun (key, count) -> leftItem = key) counts with
        | Some(k, c) -> leftItem * c
        | None -> 0

    left |> Seq.map ss |> Seq.sum

let solve lines =
    let parsed = parse lines
    let left = parsed |> Seq.map Seq.head
    let right = parsed |> Seq.map (fun s -> s |> Seq.tail |> Seq.head)
    let ansA = a left right
    let ansB = b left right
    ansA :: ansB :: []
