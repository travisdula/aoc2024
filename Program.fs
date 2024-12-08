let getData day =
    "data/" + day |> System.IO.File.ReadLines

let handleDay day solver =
    getData day |> solver |> List.map (printfn "%d") |> ignore
    0

let handleDay64 day solver =
    getData day |> solver |> List.map (printfn "%d") |> ignore
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
    | _ -> 1

[<EntryPoint>]
let main args =
    match args |> Array.toList with
    | x :: [] -> solve x
    | _ -> 1
