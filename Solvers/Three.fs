module Three
open System.Text.RegularExpressions

type Instruction =
    | Mul of int * int
    | Do
    | Dont

let getInstructions line =
    seq {
        for m in Regex.Matches(line, @"(mul\(\d+,\d+\)|do\(\)|don't\(\))") do
            yield m.Value
    }

let parseMul (str:string) =
    let arr = str[4..str.Length - 2].Split(',') |> Array.map int
    match arr with
    | [|x;y|] -> Mul(x, y)
    | _ -> failwith "should have list of length 2"

let parseInstruction (str:string) =
    match str[2] with
    | 'l' -> parseMul str
    | 'n' -> Dont
    | _ -> Do

let parse (lines: seq<string>) =
    lines
    |> Seq.collect getInstructions
    |> Seq.map parseInstruction

let allMultiply =
    function
    | Mul (x, y) -> x * y
    | _ -> 0

let a instructions =
    instructions
    |> Seq.map allMultiply
    |> Seq.sum

let folder (enabled, sum) =
    function
    | Do -> (true, sum)
    | Dont -> (false, sum)
    | Mul (x, y) -> if enabled then (true, sum + x * y) else (false, sum)

let b instructions =
    instructions
    |> Seq.fold folder (true, 0)
    |> snd

let solve lines = 
    let parsed = parse lines
    let ansA = a parsed
    let ansB = b parsed
    ansA::ansB::[]
