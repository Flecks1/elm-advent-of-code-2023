open System
open System.IO
open System.Collections.Generic


type Record =
    { Springs : Condition list
    ; Groupings : GroupSize list
    }

and Condition = char
and GroupIndex = int
and SpringIndex = int 
and GroupSize = int
and AmountOfPermutations = int64

let [<Literal>] Operational : Condition = '.'
let [<Literal>] Damaged     : Condition = '#'
let [<Literal>] Unknown     : Condition = '?'


// Parsing input

let repeatSprings amount source = seq {
    yield! source

    let mutable i = amount - 1
    while i > 0 do
        yield Unknown
        yield! source
        i <- i - 1
}
let repeatGroupings amount source = seq {
    let mutable i = amount
    while i > 0 do
        yield! source
        i <- i - 1
}

let parseLine (line: string) =
    match line.Split(" ") with
    | [| springs; groupings |] ->
        Some { Springs = springs |> repeatSprings 5 |> Seq.toList
             ; Groupings = groupings.Split(",") |> Seq.map int |> repeatGroupings 5 |> Seq.toList
             }
    | _ -> None

let getRecords () =
    Seq.tryItem 2 fsi.CommandLineArgs
    |> Option.defaultValue "..\\inputs\\day-12-input.txt"
    |> File.ReadLines
    |> Seq.choose parseLine


// Solution implementation

let getAmountOfPossiblePermutations (record: Record) =
    let springs, groupings, springsLength, groupingsLength =
        Seq.toArray record.Springs, Seq.toArray record.Groupings, Seq.length record.Springs, Seq.length  record.Groupings
    
    let mutable cache = new Dictionary<SpringIndex * GroupIndex * GroupSize, AmountOfPermutations>()

    let rec recursiveHelper springIndex groupIndex groupSize : int64 =
        if springIndex < springsLength then
            match springs.[springIndex] with
            | Operational ->
                // If we hadn't started any damaged group yet, this is fine
                if groupSize = 0 then
                    recursiveHelper (springIndex + 1) groupIndex groupSize
                
                // If the current damaged group was not fulfilled, stop here
                elif groupings.[groupIndex] <> groupSize then
                    0L

                // If the current damaged group is fulfilled, handle the next one
                else recursiveHelper (springIndex + 1) (groupIndex + 1) 0
                    
            | Damaged ->
                // If we eeren't expecting an additional damaged spring, stop here
                if groupIndex >= groupingsLength || groupings.[groupIndex] = groupSize then
                    0L

                // Otherwise, increment the size of the current damaged group
                else recursiveHelper (springIndex + 1) groupIndex (groupSize + 1)

            | Unknown ->
                match cache.TryGetValue((springIndex, groupIndex, groupSize)) with
                // We didn't find a cached answer for this specific set of input, carry on
                | false, _ ->
                    let operationalAttempt =
                        // If we haven't started a group yet, we can try treating this spring as operational
                        if groupSize = 0 then
                            recursiveHelper (springIndex + 1) groupIndex groupSize

                        // If we already have the proper size for the current group we can instead treat it as operational so we can handle the next group
                        else if groupings.[groupIndex] = groupSize then
                            recursiveHelper (springIndex + 1) (groupIndex + 1) 0

                        else 0L

                    let damagedAttempt =
                        // If the current damaged group size isn't reached, we can try handling this spring as damaged
                        if groupIndex < groupingsLength && groupSize < groupings.[groupIndex] then
                            recursiveHelper (springIndex + 1) groupIndex (groupSize + 1)

                        else 0L
                    
                    let answer = damagedAttempt + operationalAttempt
                    // Cache the answer for future runs of the same values
                    cache.Add((springIndex, groupIndex, groupSize), answer)
                    answer

                // We found a cached answer, return it directly without calculating anything
                | true, answer -> answer

            | char -> failwith ("Oops! We read an invalid spring condition that we don't know how to handle: '" + string char + "'")
        // Either we've already fullfilled the last group, or the last group ends at the end of the row
        elif groupIndex >= groupingsLength || (groupIndex = groupingsLength - 1 && groupings.[groupIndex] = groupSize) then
            1L

        // We've reached the end and we still have groups to fulfill
        else 0L

    recursiveHelper 0 0 0



let solve records =
    records
    |> Seq.map getAmountOfPossiblePermutations
    |> Seq.sum
    |> printfn "Answer: %i"



match Seq.tryItem 1 fsi.CommandLineArgs with
| Some "partTwo" ->
    printfn "Running part two solution..."
    solve (getRecords ())
| _ -> printfn "Oops! Part not specified."