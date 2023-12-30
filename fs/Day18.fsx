#r "packages/FSharpx.Collections/lib/netstandard2.0/FSharpx.Collections.dll"

open System
open System.IO
open FSharpx.Collections


let directions = {| Up = "U"; Right = "R"; Down = "D"; Left = "L" |}

type Instruction = { Direction : string; Steps : int64 }

// Parsing input

let readInputLines () =
    Seq.tryItem 2 fsi.CommandLineArgs
    |> Option.defaultValue "..\\inputs\\day-18-input.txt"
    |> File.ReadLines

let parseInputPartOne () : seq<Instruction> =
    readInputLines ()
    |> Seq.choose (fun line ->
        match line.Split(" ") with
        | [| direction; steps; _ |] -> Some { Direction = direction; Steps = int64 steps }
        | _ -> None
    )

let parseInputPartTwo () : seq<Instruction> =
    let mapDirection = function | '0' -> "R" | '1' -> "D" | '2' -> "L" | _ -> "U"
    readInputLines ()
    |> Seq.choose (fun line ->
        match line.Split(" ") with
        | [| _; _; hexCode |] -> Some { Direction = mapDirection(hexCode[7]); Steps = Convert.ToInt64(hexCode[2..6], 16) }
        | _ -> None
    )


// Main script

let handleInstructions (instructions: seq<Instruction>) =
    let handleInstruction (area, pointsInPerimeter, y) instruction =
        ( if instruction.Direction = directions.Right
          then area + y * instruction.Steps
          elif instruction.Direction = directions.Left
          then area - y * instruction.Steps
          else area
        , pointsInPerimeter + instruction.Steps
        , if instruction.Direction = directions.Up
          then y + instruction.Steps
          elif instruction.Direction = directions.Down
          then y - instruction.Steps
          else y
        )

    let area, pointsInPerimeter, _ = instructions |> Seq.fold handleInstruction (0L, 0L, 0L)
    area, pointsInPerimeter
    

let solve (instructions : seq<Instruction>) =
    let calculatedArea, pointsInPerimeter  = handleInstructions instructions
    let pointsInsidePerimeter = calculatedArea - pointsInPerimeter / 2L + 1L
    printfn "Answer: %i" (pointsInsidePerimeter + pointsInPerimeter)



match Seq.tryItem 1 fsi.CommandLineArgs with
| Some "partOne" ->
    printfn "Running part one solution..."
    solve <| parseInputPartOne ()
| Some "partTwo" ->
    printfn "Running part two solution..."
    solve <| parseInputPartTwo ()
| _ -> printfn "Oops! Part not specified."

