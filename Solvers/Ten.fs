module Ten

let parse (lines: string seq) =
    let maprow r row =
        Seq.mapi (fun c ch -> ((r, c), int ch - int '0')) row

    lines |> Seq.mapi maprow |> Seq.concat |> Map.ofSeq

let offsets = [ (0, 1); (1, 0); (-1, 0); (0, -1) ]

let score map pos height =
    let rec loop (r, c) h =
        if h = 9 then
            Set.singleton (r, c)
        else
            offsets
            |> List.map (fun (dr, dc) -> (dr + r, dc + c))
            |> List.choose (fun k ->
                match Map.tryFind k map with
                | Some v when v = h + 1 -> Some(k, v)
                | _ -> None)
            |> List.map (fun (k, v) -> loop k v)
            |> function
                | [] -> Set.empty
                | xs -> Set.unionMany xs

    let heads = loop pos height
    heads |> Set.count

let rating map pos height =
    let rec loop (r, c) h =
        if h = 9 then
            1
        else
            offsets
            |> List.map (fun (dr, dc) -> (dr + r, dc + c))
            |> List.choose (fun k ->
                match Map.tryFind k map with
                | Some v when v = h + 1 -> Some(k, v)
                | _ -> None)
            |> List.map (fun (k, v) -> loop k v)
            |> List.sum

    let s = loop pos height
    printfn "score(%A) = %A" pos s |> ignore
    s

let a map =
    map
    |> Map.filter (fun k v -> v = 0)
    |> Map.map (score map)
    |> Map.toSeq
    |> Seq.map snd
    |> Seq.sum

let b map =
    map
    |> Map.filter (fun k v -> v = 0)
    |> Map.map (rating map)
    |> Map.toSeq
    |> Seq.map snd
    |> Seq.sum

let solve input =
    let parsed = parse input
    let ansA = a parsed
    let ansB = b parsed
    (ansA, ansB)
