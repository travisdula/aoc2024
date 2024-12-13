let getData day =
    "data/" + day |> System.IO.File.ReadLines

let getBothData day =
    let short = "data/" + day + "_short" |> System.IO.File.ReadLines
    let long = "data/" + day |> System.IO.File.ReadLines
    (short, long)

let handleDay day solver =
    getData day |> solver |> List.iter (printfn "%d")
    0

let handleDay64 day (solver: string seq -> int64 list) =
    getData day |> solver |> List.iter (printfn "%d")
    0

let multiHandler day solver =
    let (short, long) = getBothData day
    let (ashort, bshort) = solver short
    printfn "Day %A (short): %A %A" (int day) ashort bshort
    let (afull, bfull) = solver long
    printfn "Day %A (full): %A %A" (int day) afull bfull
    0

let solve day =
    let handler = handleDay day

    match day with
    | "1" -> handler One.solve
    | "2" -> handler Two.solve
    | "3" -> handler Three.solve
    | "4" -> handler Four.solve
    | "5" -> handler Five.solve
    | "6" -> handler Six.solve
    | "7" -> handleDay64 day Seven.solve
    | "8" -> handler Eight.solve
    | "9" -> handleDay64 day Nine.solve
    | "10" -> multiHandler day Ten.solve
    | "11" -> multiHandler day Eleven.solve
    | "12" -> multiHandler day Twelve.solve
    | "13" -> multiHandler day Thirteen.solve
    | _ -> 1

[<EntryPoint>]
let main args =
    match args with
    | [| x |] -> solve x
    | _ -> 1
