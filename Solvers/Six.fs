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

let a lab =
    let rec loop (guard: Guard) (seen: Position Set) =
        let ifFwd = moveForward guard
        let (dir, pos) = guard
        let newSeen = Set.add pos seen

        match Map.tryFind ifFwd lab with
        | None -> newSeen
        | Some o ->
            let newGuard =
                match o with
                | '#' -> turn guard
                | _ -> (dir, ifFwd)

            loop newGuard newSeen

    let startPos = Map.findKey (fun _ v -> v = '^') lab
    loop (Up, startPos) Set.empty |> Set.count

let b lab =
    let rec loop (guard: Guard) (seen: Guard Set) blockedLab =
        if Set.contains guard seen then
            true
        else
            let ifFwd = moveForward guard
            let newSeen = Set.add guard seen
            let (dir, pos) = guard

            match Map.tryFind ifFwd blockedLab with
            | None -> false
            | Some o ->
                let newGuard =
                    match o with
                    | '#' -> turn guard
                    | _ -> (dir, ifFwd)

                loop newGuard newSeen blockedLab

    let startPos = Map.findKey (fun _ v -> v = '^') lab
    let predicate = loop (Up, startPos) Set.empty

    let labWithBlock loc = Map.add loc '#' lab |> predicate

    lab |> Map.filter (fun k v -> v = '.' && labWithBlock k) |> Map.count

let solve lines =
    let parsed = parse lines
    let ansA = a parsed
    let ansB = b parsed
    ansA :: ansB :: []
