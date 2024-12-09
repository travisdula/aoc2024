module Nine

type blocks =
    | FreeBlocks of int64
    | FilledBlocks of (int64 * int64) // (id, len)

type block =
    | Free
    | Filled of int64

let indexedNumberToBlock i x =
    if i % 2 = 0 then
        FilledBlocks((int64 i) / 2L, x)
    else
        FreeBlocks x

let parse (lines: string seq) =
    lines
    |> Seq.head
    |> Seq.map int64
    |> Seq.map ((+) -48L)
    |> Seq.mapi indexedNumberToBlock


let seqify disk =
    let flattenBlock =
        function
        | FilledBlocks(ide, len) -> Seq.replicate (int len) (Filled ide)
        | FreeBlocks len -> Seq.replicate (int len) Free

    disk |> Seq.collect flattenBlock

let rearrange (result, backwards, count) elem =
    match (count, elem, backwards) with
    | 0L, _, _ -> (result, backwards, count)
    | _, Filled x, _ -> (x :: result, backwards, count - 1L)
    | _, Free, Filled x :: xs -> (x :: result, xs, count - 1L)
    | _ -> failwith "impossible"

let removeTrailingFree disk =
    match Seq.tryLast disk with
    | Some(FreeBlocks _) -> Seq.take (Seq.length disk - 1) disk
    | _ -> disk

let blocksId =
    function
    | FreeBlocks len -> 0L
    | FilledBlocks(i, len) -> i

let blockId =
    function
    | Free -> 0L
    | Filled i -> i

let atLeastNFree n =
    function
    | FreeBlocks x -> x >= n
    | _ -> false

let seqTryFindIndexItem pred s =
    match Seq.tryFindIndex pred s with
    | None -> None
    | Some index -> Some(index, Seq.item index s)

let clean disk =
    let cleaner state elem =
        match elem with
        | FilledBlocks(_, _) -> elem :: state
        | FreeBlocks 0L -> state
        | FreeBlocks n ->
            match state with
            | (FreeBlocks m) :: xs -> (FreeBlocks(n + m)) :: xs
            | _ -> (FreeBlocks n) :: state

    Seq.fold cleaner [] disk |> List.rev |> Seq.ofList

let a disk =
    let withoutEmpty =
        Seq.filter
            (function
            | Free -> false
            | _ -> true)
            (seqify disk)
        |> Seq.toList
        |> List.rev

    let valid = withoutEmpty |> Seq.length |> int64
    let state = ([], withoutEmpty, valid)

    disk
    |> seqify
    |> Seq.fold rearrange state
    |> fun (a, b, c) -> a
    |> List.rev
    |> List.mapi (fun i x -> (int64 i) * x)
    |> List.sum

let b disk =
    let rec loop defrag i =
        if i = 0 then
            defrag
        else
            let newI = i - 1

            match seqTryFindIndexItem (blocksId >> (=) i) defrag with
            | Some(toMoveIndex, FilledBlocks(_, lenFilled)) ->
                match seqTryFindIndexItem (atLeastNFree lenFilled) defrag with
                | Some(freeIndex, FreeBlocks lenFree) when freeIndex < toMoveIndex ->
                    let newDefrag =
                        defrag
                        |> Seq.updateAt freeIndex (FilledBlocks(i, lenFilled))
                        |> Seq.updateAt toMoveIndex (FreeBlocks lenFilled)
                        |> Seq.insertAt (freeIndex + 1) (FreeBlocks(lenFree - lenFilled))
                        |> clean

                    loop newDefrag newI
                | _ -> loop defrag newI
            | _ -> loop defrag newI

    let maxId = disk |> Seq.maxBy blocksId |> blocksId |> int

    loop (removeTrailingFree disk) maxId
    |> seqify
    |> Seq.mapi (fun i x -> (int64 i) * (blockId x))
    |> Seq.sum

let solve lines =
    let parsed = parse lines
    let ansA = a parsed
    let ansB = b parsed
    ansA :: ansB :: []
