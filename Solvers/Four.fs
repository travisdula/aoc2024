module Four

let parse = Seq.toArray

let offsets = 
    [
        [(0, 0) ; (1, 0) ; (2, 0) ; (3, 0)]
         ; [(3, 0) ; (2, 0) ; (1, 0) ; (0, 0)]
         ; [(0, 0) ; (0, 1) ; (0, 2) ; (0, 3)]
         ; [(0, 3) ; (0, 2) ; (0, 1) ; (0, 0)]
         ; [(0, 0) ; (1, 1) ; (2, 2) ; (3, 3)]
         ; [(3, 0) ; (2, 1) ; (1, 2) ; (0, 3)]
         ; [(0, 3) ; (1, 2) ; (2, 1) ; (3, 0)]
         ; [(3, 3) ; (2, 2) ; (1, 1) ; (0, 0)]
    ]

let check (grid: string array) r c offset =
    try
        List.map (fun (a, b) -> grid[r + a][c + b]) offset
        |> List.map string
        |> String.concat ""
        |> (=) "XMAS"
    with
        | ex -> false


let xmasCount grid (r, c) = 
    List.map (check grid r c) offsets |> List.filter id |> List.length

let checkMasX (grid: string array) (r, c) =
    grid[r][c] = 'A'
    && ((grid[r + 1][c + 1] = 'S' && grid[r - 1][c - 1] = 'M') || (grid[r + 1][c + 1] = 'M' && grid[r - 1][c - 1] = 'S'))
    && ((grid[r + 1][c - 1] = 'S' && grid[r - 1][c + 1] = 'M') || (grid[r + 1][c - 1] = 'M' && grid[r - 1][c + 1] = 'S'))

let a (grid: string array): int =
    seq { 0..grid.Length }
    |> Seq.collect (fun r -> seq { 0..grid[0].Length } |> Seq.map (fun c -> (r, c)))
    |> Seq.map (xmasCount grid)
    |> Seq.sum

let b (grid: string array) =
    seq { 1..grid.Length-2 }
    |> Seq.collect (fun r -> seq { 1..grid[0].Length-2 } |> Seq.map (fun c -> (r, c)))
    |> Seq.filter (checkMasX grid)
    |> Seq.length

let solve lines = 
    let parsed = parse lines
    let ansA = a parsed
    let ansB = b parsed
    ansA::ansB::[]
