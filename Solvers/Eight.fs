module Eight

let maprow r row =
    row |>
    Seq.mapi (fun c ch -> ((r,c),ch))

let parse lines =
    lines
    |> Seq.mapi maprow
    |> Seq.concat
    |> Map.ofSeq

let mapPairwise map =
    Seq.allPairs (Map.toSeq map) (Map.toSeq map)

let antiNodes ((r1, c1), (r2, c2)) =
    let dr = r1 - r2
    let dc = c1 - c2
    seq {
        (r1 + dr, c1 + dc)
        (r2 - dr, c2 - dc)
    }

let antennaPairs map =
    map
    |> Map.filter (fun k v -> v <> '.')
    |> mapPairwise
    |> Seq.filter (fun ((p1, c1), (p2, c2)) -> p1 <> p2 && c1 = c2)
    |> Seq.map (fun ((p1, c1), (p2, c2)) -> (p1, p2))

let a map =
    map
    |> antennaPairs
    |> Seq.collect antiNodes
    |> Set.ofSeq
    |> Set.intersect (Set.ofSeq <| Map.keys map)
    |> Set.count

let b inp =
    0

let solve lines =
    let parsed = parse lines
    let ansA = a parsed
    let ansB = b parsed
    ansA :: ansB :: []
