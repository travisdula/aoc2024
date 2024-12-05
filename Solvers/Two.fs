module Two

let parse (lines: seq<string>) =
    lines |> Seq.map (_.Split(" ") >> Seq.map int)

let isSafe report =
    let pairs = Seq.pairwise report
    let increasing = Seq.forall (fun (x, y) -> x < y) pairs
    let decreasing = Seq.forall (fun (x, y) -> x > y) pairs
    let close = Seq.forall (fun (x, y) -> abs (x - y) <= 3) pairs
    close && (increasing || decreasing)

let isSafeWithDampening report =
    let dampened =
        seq { 0 .. (Seq.length report - 1) } |> Seq.map (fun i -> Seq.removeAt i report)

    isSafe report || Seq.exists isSafe dampened

let a reports =
    reports |> Seq.filter isSafe |> Seq.length

let b reports =
    reports |> Seq.filter isSafeWithDampening |> Seq.length

let solve lines =
    let parsed = parse lines
    let ansA = a parsed
    let ansB = b parsed
    ansA :: ansB :: []
