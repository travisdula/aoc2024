module Seven

let parseLine line =
    let solution = Seq.takeWhile ((<>) ':') line |> System.String.Concat |> int64

    let parameters =
        Seq.skipWhile ((<>) ':') line
        |> Seq.skip 2
        |> System.String.Concat
        |> _.Split(' ')
        |> Seq.ofArray
        |> Seq.map int64

    (solution, parameters)

let parse (lines: string seq) = lines |> Seq.map parseLine

let concatNumbers a b = string a + string b |> int64

let genericSoln ops eqns =
    let checkEqn (sol, par) =
        let folder state elem =
            state
            |> Set.toSeq
            |> Seq.allPairs ops
            |> Seq.map (fun (f, a) -> f a elem)
            |> Set.ofSeq
        let state = Set.singleton <| Seq.head par
        let source = Seq.tail par

        Seq.fold folder state source |> Set.contains sol
    eqns |> Seq.filter checkEqn |> Seq.map fst |> Seq.sum

let a =
    seq {
        (+)
        (*)
    }
    |> genericSoln

let b =
    seq {
        (+)
        (*)
        concatNumbers
    }
    |> genericSoln

let solve lines =
    let parsed = parse lines
    let ansA = a parsed
    let ansB = b parsed
    ansA :: ansB :: []
