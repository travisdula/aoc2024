module Thirteen

let parseButton (button: string) =
    let [| left; y |] = button.Split(", Y+")
    let [| _; x |] = left.Split(": X+")
    (int64 x, int64 y)

let parsePrize (prize: string) =
    let [| x; y |] = prize.Replace("Prize: X=", "").Split(", Y=")
    (int64 x, int64 y)


let parseMachine lines =
    let [ btnA; btnB; prize ] = Seq.toList lines |> List.take 3
    (parseButton btnA, parseButton btnB, parsePrize prize)


let parse lines =
    lines |> Seq.chunkBySize 4 |> Seq.map parseMachine

let solveEquation ((ax, ay), (bx, by), (x, y)) =
    let a =
        (float x - float bx * float y / float by)
        / (float ax - float bx * float ay / float by)
        |> round
        |> int64

    let b =
        (float y - float ay * float x / float ax)
        / (float by - float ay * float bx / float ax)
        |> round
        |> int64

    if (ax * a + bx * b, ay * a + by * b) = (x, y) then
        Some(a, b)
    else
        None

let cost (a, b) = 3L * a + b

let offset: int64 = 10000000000000L

let a input =
    input |> Seq.choose solveEquation |> Seq.map cost |> Seq.sum |> Some

let b input =
    input
    |> Seq.choose (fun (ap, bp, (x, y)) -> solveEquation (ap, bp, (x + offset, y + offset)))
    |> Seq.map cost
    |> Seq.sum
    |> Some

let solve input =
    let parsed = parse input
    (a parsed, b parsed)
