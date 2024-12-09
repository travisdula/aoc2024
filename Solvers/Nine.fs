module Nine

type blocks = 
    | FreeBlocks of int64 
    | FilledBlocks of (int64 * int64) // (id, len)

type block = Free | Filled of int64

let indexedNumberToBlock i x =
    if (int64 i) % 2L = 0L
    then FilledBlocks((int64 i) / 2L, x)
    else FreeBlocks x
let parse (lines: string seq) =
    lines
    |> Seq.head
    |> Seq.map int64
    |> Seq.map ((+) -48L)
    |> Seq.mapi  indexedNumberToBlock

let flattenBlock =
    function
    | FilledBlocks (ide, len) -> Seq.replicate (int len) (Filled ide)
    | FreeBlocks len -> Seq.replicate (int len) Free
let seqify disk =
    disk |> Seq.collect flattenBlock

let rearrange (result, backwards, count) elem =
    match (count, elem, backwards) with
    | 0L, _, _ -> (result, backwards, count)
    | _, Filled x, _ -> (x::result, backwards, count - 1L)
    | _, Free, Filled x:: xs -> (x::result, xs, count - 1L)
    | _ -> failwith "impossible"
           

let a disk =
    let flatDisk = disk |> seqify
    let withoutEmpty = Seq.filter (function Free -> false | _ -> true) (seqify disk) |> Seq.toList |> List.rev
    let valid = withoutEmpty |> Seq.length
    flatDisk
    |> Seq.fold rearrange ([], withoutEmpty, valid)
    |> fun (a, b, c) -> a
    |> List.rev
    |> List.mapi (fun i x -> (int64 i) * x)
    |> List.sum

let b inp =
    0L

let solve lines =
    let parsed = parse lines
    let ansA = a parsed
    let ansB = b parsed
    ansA :: ansB :: []
