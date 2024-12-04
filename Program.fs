﻿let readLines filePath = System.IO.File.ReadLines filePath
let getData day = "data/" + day |> readLines
let handleDay day solver = 
    getData day |> solver |> List.map (printfn "%d") |> ignore
    0

let solve day =
    match day with
    | "1" -> handleDay day One.solve
    | _ -> 1

[<EntryPoint>]
let main args =
    match args |> Array.toList with
    | x::[] -> solve x
    | _ -> 1
