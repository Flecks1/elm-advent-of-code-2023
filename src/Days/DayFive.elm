module Days.DayFive exposing (first, second)

import Html exposing (a)
import List.Extra as List
import Maybe.Extra as Maybe
import Parser exposing ((|.), (|=), Parser, Step(..))
import Puzzle


first : Puzzle.Solution
first =
    parseAlmanach seedSimpleListParser
        >> Result.andThen
            (\maps ->
                maps.seeds
                    |> List.map (convertSeedToLocation maps)
                    |> List.minimum
                    |> Maybe.map String.fromInt
                    |> Result.fromMaybe "Couldn't find an answer!?"
            )


second : Puzzle.Solution
second input =
    -- TODO: add ports / custom update function for this one so we can offload the task of keeping up with currently handled chunk and the remaining ones to the consuming website
    case parseAlmanach seedRangeListParser input of
        Ok almanach ->
            let
                amountOfChunks =
                    List.length almanach.seeds
            in
            almanach.seeds
                |> List.foldl (findMinimumLocationFromSeedRange almanach) { minimum = 2147483646, n = 0, amountOfChunks = amountOfChunks }
                |> .minimum
                |> String.fromInt
                |> Ok

        Err err ->
            Err err


findMinimumLocationFromSeedRange : Almanach a -> SeedRange -> { minimum : Int, n : Int, amountOfChunks : Int } -> { minimum : Int, n : Int, amountOfChunks : Int }
findMinimumLocationFromSeedRange maps ({ start, max } as range) info =
    if start == max then
        let
            _ =
                Debug.log "" { minimum = info.minimum, percentage = toFloat (info.n + 1) / toFloat info.amountOfChunks * 100.0 }
        in
        { info | n = info.n + 1 }

    else
        findMinimumLocationFromSeedRange maps { range | start = start + 1 } { info | minimum = min (convertSeedToLocation maps start) info.minimum }


convertSeedToLocation : Almanach a -> Int -> Int
convertSeedToLocation maps seed =
    seed |> convert maps.seedToSoil |> convert maps.soilToFert |> convert maps.fertToWata |> convert maps.wataToLite |> convert maps.liteToTemp |> convert maps.tempToHumy |> convert maps.humyToLcat


convert : List MapDefinition -> Int -> Int
convert definitions source =
    let
        matchingDefinition =
            definitions |> List.find (\{ sourceStart, rangeLength } -> source >= sourceStart && source < sourceStart + rangeLength)
    in
    case matchingDefinition of
        Just { sourceStart, destinationStart } ->
            source - sourceStart + destinationStart

        Nothing ->
            source


type alias Almanach seedType =
    { seeds : List seedType
    , seedToSoil : List MapDefinition
    , soilToFert : List MapDefinition
    , fertToWata : List MapDefinition
    , wataToLite : List MapDefinition
    , liteToTemp : List MapDefinition
    , tempToHumy : List MapDefinition
    , humyToLcat : List MapDefinition
    }


type alias SeedRange =
    { start : Int, max : Int }


type alias MapDefinition =
    { destinationStart : Int
    , sourceStart : Int
    , rangeLength : Int
    }


mapDefinitionsParser : Parser (List MapDefinition)
mapDefinitionsParser =
    Parser.loop []
        (\definitions ->
            Parser.oneOf
                [ Parser.succeed (\x y z -> MapDefinition x y z :: definitions |> Loop)
                    |= Parser.int
                    |. Parser.spaces
                    |= Parser.int
                    |. Parser.spaces
                    |= Parser.int
                    |. Parser.symbol "\n"
                    |> Parser.backtrackable
                , Parser.symbol "\n" |> Parser.map (\_ -> definitions |> List.sortBy .sourceStart |> Done) |> Parser.backtrackable
                ]
        )


seedSimpleListParser : Parser (List Int)
seedSimpleListParser =
    Parser.loop []
        (\seeds ->
            Parser.oneOf
                [ Parser.succeed (\seed -> seed :: seeds |> Loop)
                    |. Parser.symbol " "
                    |= Parser.int
                    |> Parser.backtrackable
                , Parser.symbol "\n" |> Parser.map (\_ -> seeds |> Done) |> Parser.backtrackable
                ]
        )


chunkUpRanges : List SeedRange -> List SeedRange
chunkUpRanges ranges =
    let
        chunkSize =
            50000

        makeChunks chunks { start, max } =
            if max - start <= chunkSize then
                { start = start, max = max } :: chunks

            else
                makeChunks ({ start = start, max = start + chunkSize } :: chunks) { start = start + chunkSize, max = max }
    in
    ranges |> List.concatMap (makeChunks [])


seedRangeListParser : Parser (List SeedRange)
seedRangeListParser =
    Parser.loop []
        (\seeds ->
            Parser.oneOf
                [ Parser.succeed (\start length -> SeedRange start (start + length) :: seeds |> Loop)
                    |. Parser.symbol " "
                    |= Parser.int
                    |. Parser.symbol " "
                    |= Parser.int
                    |> Parser.backtrackable
                , Parser.symbol "\n" |> Parser.map (\_ -> seeds |> chunkUpRanges |> Done) |> Parser.backtrackable
                ]
        )


parseAlmanach : Parser (List seedType) -> String -> Result String (Almanach seedType)
parseAlmanach seedsParser =
    let
        parser =
            Parser.succeed Almanach
                |. Parser.token "seeds:"
                |= seedsParser
                |. Parser.symbol "\n"
                |. Parser.token "seed-to-soil map:\n"
                |= mapDefinitionsParser
                |. Parser.token "soil-to-fertilizer map:\n"
                |= mapDefinitionsParser
                |. Parser.token "fertilizer-to-water map:\n"
                |= mapDefinitionsParser
                |. Parser.token "water-to-light map:\n"
                |= mapDefinitionsParser
                |. Parser.token "light-to-temperature map:\n"
                |= mapDefinitionsParser
                |. Parser.token "temperature-to-humidity map:\n"
                |= mapDefinitionsParser
                |. Parser.token "humidity-to-location map:\n"
                |= mapDefinitionsParser
    in
    Parser.run parser >> Result.mapError (Debug.log "deadEnds" >> (\_ -> "Parser error, look in the console"))
