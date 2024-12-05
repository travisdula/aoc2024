module Five

let hasPipe (s: string) = s.Contains('|')
let hasNoComma (s: string) = not (s.Contains(','))

let pairArrTuple (arr: 'a array) : ('a * 'a) =
    match arr with
    | [| a; b |] -> (a, b)
    | _ -> failwith "invalid input length"

let parse (lines: string seq) =
    let rules =
        Seq.takeWhile hasPipe lines
        |> Seq.map (fun s -> s.Split('|'))
        |> Seq.map pairArrTuple

    let pages =
        Seq.skipWhile hasNoComma lines
        |> Seq.map (fun s -> s.Split(','))
        |> Seq.map Seq.ofArray

    (rules, pages)

let correctlyOrdered table update =
    let updateSet = Set.ofSeq update

    let f (seen, valid) elem =
        let nextSeen = Set.add elem seen

        if valid then
            match Map.tryFind elem table with
            | Some predecessors -> (nextSeen, Set.intersect (predecessors - seen) updateSet = Set.empty)
            | None -> (nextSeen, true)
        else
            (nextSeen, false)

    let s = (Set.empty, true)
    update |> Seq.fold f s |> snd

let middle s = Seq.item (Seq.length s / 2) s

let rulesMap rules =
    let folder table (before, after) =
        let prior = Map.tryFind after table |> Option.defaultValue Set.empty
        Map.add after (Set.add before prior) table

    Seq.fold folder Map.empty rules

let a (rules, pages) =
    pages
    |> Seq.filter (correctlyOrdered <| rulesMap rules)
    |> Seq.map (middle >> int)
    |> Seq.sum

let kahns S edges =
    let rec go (S: 'a Set) (L: 'a list) (graph: ('a * 'a) seq) =
        if S = Set.empty then
            L
        else
            let min = Set.minElement S
            let eachNode = Seq.filter (fst >> (=) min) graph |> Seq.map snd
            let newGraph = Seq.filter (fst >> (<>) min) graph

            let newS =
                eachNode
                |> Seq.filter (fun node -> Seq.forall (fun (n, m) -> m <> node) newGraph)
                |> Set.ofSeq
                |> Set.union (Set.remove min S)

            go newS (min :: L) newGraph

    go S [] edges |> List.rev |> Seq.ofList

let indexer sequence elem =
    Seq.tryFindIndex ((=) elem) sequence |> Option.defaultValue -1

let b (rules, pages) =
    let map = rulesMap rules

    let fix update =
        let updateSet = Set.ofSeq update

        let appliedRules =
            rules
            |> Seq.filter (fun (a, b) -> [ a; b ] |> Set.ofList |> Set.isSuperset updateSet)

        let left = Seq.map fst appliedRules |> Set.ofSeq
        let right = Seq.map snd appliedRules |> Set.ofSeq
        let S = left - right
        let key = kahns S appliedRules |> indexer
        Seq.sortBy key update

    pages
    |> Seq.filter (correctlyOrdered map >> not)
    |> Seq.map fix
    |> Seq.map (fix >> middle >> int)
    |> Seq.sum

let solve lines =
    let parsed = parse lines
    let ansA = a parsed
    let ansB = b parsed
    ansA :: ansB :: []
