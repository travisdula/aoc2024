module Six

type Direction =
    | Up
    | Down
    | Left
    | Right

type Position = int * int

type Guard = Direction * Position

let moveForward ((dir, (r, c)): Guard) =
    match dir with
    | Up -> (r - 1, c)
    | Down -> (r + 1, c)
    | Left -> (r, c - 1)
    | Right -> (r, c + 1)

let turn ((dir, pos): Guard) : Guard =
    match dir with
    | Up -> (Right, pos)
    | Down -> (Left, pos)
    | Right -> (Down, pos)
    | Left -> (Up, pos)

let rowToMapping r row =
    let charToMapping c ch = ((r, c), ch)
    Seq.mapi charToMapping row

let parse (lines: string seq) =
    lines |> Seq.mapi rowToMapping |> Seq.concat |> Map.ofSeq

let rec loop (guard: Guard) (seen: Guard Set) lab =
    if Set.contains guard seen then
        None
    else
        let ifFwd = moveForward guard
        let (dir, pos) = guard
        let newSeen = Set.add guard seen

        match Map.tryFind ifFwd lab with
        | None -> Some newSeen
        | Some o ->
            let newGuard =
                match o with
                | '#' -> turn guard
                | _ -> (dir, ifFwd)

            loop newGuard newSeen lab

let a lab =
    let startPos = Map.findKey (fun _ v -> v = '^') lab

    match loop (Up, startPos) Set.empty lab with
    | Some s -> Set.map snd s |> Set.count
    | None -> failwith "guard could not escape"

let b lab =
    let startPos = Map.findKey (fun _ v -> v = '^') lab

    let labWithBlock loc =
        Map.add loc '#' lab |> loop (Up, startPos) Set.empty

    lab
    |> Map.filter (fun k v -> v = '.' && Option.isNone <| labWithBlock k)
    |> Map.count

let solve lines =
    let parsed = parse lines
    let ansA = a parsed
    let ansB = b parsed
    ansA :: ansB :: []
