#r "packages/FSharpx.Collections/lib/netstandard2.0/FSharpx.Collections.dll"

open System
open System.IO
open FSharpx.Collections

type Heatloss = int
type Steps = int
type Position = int * int
type Direction = int * int
module Direction =
    let up = 0, -1
    let right = 1, 0
    let down = 0, 1
    let left = -1, 0
    let none = 0, 0


type Heatmap = Map<Position, Heatloss>

type Node = Position * Direction * Steps
type NodeState = Heatloss * Node

type PathfindingState =
    { Destination : Position
    ; Heatmap : Heatmap
    ; ToWalk : Heap<Heatloss * Node>
    ; Seen : Set<Node>
    }


// Parsing input

let parseHeatmapFromInputLines (input: string seq) : Heatmap =
    let zeroCharCode = int '0'
    let handleSymbol (state: Heatmap) x y digit =
        state |> Map.add (x, y) (int digit - zeroCharCode)
    let handleRow (state: Heatmap) (y, row) =
        Seq.indexed row |> Seq.fold (fun s (x, digit) -> handleSymbol s x y digit) state
    Seq.indexed input |> Seq.fold handleRow Map.empty

let getHeatmap () =
    Seq.tryItem 2 fsi.CommandLineArgs
    |> Option.defaultValue "..\\inputs\\day-17-input.txt"
    |> File.ReadLines
    |> parseHeatmapFromInputLines


// Helpers

let getDestination heatmap = try Map.maxKeyValue heatmap |> fst with _ -> 0, 0

let addDirection times ( deltaX, deltaY ) ( x, y )  =
    x + deltaX * times, y + deltaY * times

let range i j = seq { i .. j }


// Pathfinding implementation

let getNeighbors minSteps maxSteps heatmap ( currentHeatloss, ( currentPosition, currentDirection, _ ) ) =
    let directions =
        // We were going left or right, get the neighbors on the top or bottom
        if currentDirection = Direction.left || currentDirection = Direction.right then [ Direction.up; Direction.down ]
        // We were going up or down, get the neighbors on the left and right
        elif currentDirection = Direction.up || currentDirection = Direction.down then [ Direction.left; Direction.right ]
        // We're at the start, get the neighbors on the right and bottom
        else [ Direction.right; Direction.down ]
    
    let makeNeighbors direction =
        // Try to get the neighbors for each amount of steps possible,
        // while making sure to account for the unaccessible neighbors' heatloss if minSteps > 0
        let folder ( neighbors, totalHeatloss ) steps =
            let neighborPosition = currentPosition |> addDirection steps direction
            let neighborNode = neighborPosition, direction, steps
            match Map.tryFind neighborPosition heatmap with
            | Some neighborHeatloss ->
                let newHeatloss = totalHeatloss + neighborHeatloss
                ( if steps >= minSteps then ( newHeatloss, neighborNode ) :: neighbors else neighbors
                , newHeatloss
                )
            | None -> ( neighbors, totalHeatloss )
        range 1 maxSteps |> Seq.fold folder ([], currentHeatloss) |> fst

    directions |> Seq.collect makeNeighbors

let rec findCheapestPathHeatloss minSteps maxSteps state =
    let handleNeighbor currentState ( ( _, neighborNode ) as neighborNodeState ) =
        // Only add the new neighbor to the heap if we haven't been to it before in the same manner (travelling in the same direction and with the same amount of steps)
        if not (Set.contains neighborNode state.Seen) then
            { currentState with ToWalk = currentState.ToWalk |> Heap.insert neighborNodeState; Seen = currentState.Seen |> Set.add neighborNode }
        else currentState

    match Heap.tryUncons state.ToWalk with
    | Some ( ( ( currentHeatloss, ( currentPosition, _, _ ) ) as currentNodeState), remainingToWalk ) ->
        if currentPosition <> state.Destination then
            getNeighbors minSteps maxSteps state.Heatmap currentNodeState
            |> Seq.fold handleNeighbor { state with ToWalk = remainingToWalk }
            |> findCheapestPathHeatloss minSteps maxSteps
        else printfn "Cheapest path cost found : %i" currentHeatloss
    | None -> printfn "Oops! Destination was never reached. This should not happen."


// Main script

let solve minSteps maxSteps heatmap =
    findCheapestPathHeatloss minSteps maxSteps
        { Destination = getDestination heatmap
        ; Heatmap = heatmap
        ; ToWalk = Heap.ofSeq false [ 0, ( ( 0, 0 ), Direction.none, 0 ) ]
        ; Seen = Set.empty
        }

match Seq.tryItem 1 fsi.CommandLineArgs with
| Some "partOne" ->
    printfn "Running part one solution..."
    solve 1 3 (getHeatmap ())
| Some "partTwo" ->
    printfn "Running part two solution..."
    solve 4 10 (getHeatmap ())
| _ -> printfn "Oops! Part not specified."

