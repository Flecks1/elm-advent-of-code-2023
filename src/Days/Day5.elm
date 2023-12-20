module Days.Day5 exposing (first, second)

import Html exposing (a, source)
import List exposing (range)
import List.Extra as List
import Maybe.Extra as Maybe
import Parser exposing ((|.), (|=), Parser, Step(..))
import Puzzle
import Utils.Various exposing (iff)


first : Puzzle.Solution
first =
    let
        useMappings : List MappingRange -> Int -> Int
        useMappings mappingRanges source =
            let
                matchingDefinition =
                    mappingRanges |> List.find (\{ sourceStart, rangeLength } -> source >= sourceStart && source < sourceStart + rangeLength)
            in
            case matchingDefinition of
                Just { sourceStart, destinationStart } ->
                    source - sourceStart + destinationStart

                Nothing ->
                    source

        convertSeedToLocation maps =
            useMappings maps.seedToSoil
                >> useMappings maps.soilToFertilizer
                >> useMappings maps.fertilizerToWater
                >> useMappings maps.waterToLight
                >> useMappings maps.lightToTemperature
                >> useMappings maps.temperatureToHumidity
                >> useMappings maps.humidityToLocation
    in
    parseAlmanach seedSimpleListParser
        >> Result.andThen
            (\almanach ->
                almanach.seeds
                    |> List.map (convertSeedToLocation almanach)
                    |> List.minimum
                    |> Maybe.map String.fromInt
                    |> Result.fromMaybe "Couldn't find an answer!?"
            )


second : Puzzle.Solution
second input =
    let
        splitAndMapRange { sourceStart, destinationStart, rangeLength } { mappedRanges, start, end } =
            let
                -- start = 79, end = 93
                -- sourceStart = 50, destinationStart = 52, rangeLength = 48
                -- mappingRangeEnd = 98
                mappingRangeEnd =
                    sourceStart + rangeLength

                unmappedRange =
                    if start < sourceStart then
                        Just (Range start (min end (sourceStart - 1)))

                    else
                        Nothing

                mappedRange =
                    if start <= mappingRangeEnd && end >= sourceStart then
                        Just (Range (max start sourceStart - sourceStart + destinationStart) (destinationStart + min (end - sourceStart) rangeLength))

                    else
                        Nothing
            in
            iff (end > mappingRangeEnd) List.Continue List.Stop <|
                { mappedRanges = List.filterMap identity [ unmappedRange, mappedRange ] ++ mappedRanges
                , start = min end (sourceStart + rangeLength + 1)
                , end = end
                }

        mapRange mappingRanges { start, end } =
            let
                _ =
                    Debug.log "mapped range" { start = start, end = end, mappedRanges = mappedRanges }

                mappedRanges =
                    mappingRanges |> List.stoppableFoldl splitAndMapRange { mappedRanges = [], start = start, end = end } |> .mappedRanges
            in
            mappedRanges

        recursivelyMapRanges mappings ranges =
            case mappings of
                mappingRanges :: restOfMappingRanges ->
                    let
                        _ =
                            Debug.log "using a new mapping!" mappingRanges
                    in
                    List.concatMap (mapRange mappingRanges) ranges
                        |> List.sortBy .start
                        |> recursivelyMapRanges restOfMappingRanges

                [] ->
                    ranges |> List.minimumBy .start |> Maybe.unwrap 0 .start |> String.fromInt |> Ok
    in
    -- This one takes STUPIDLY long to run
    case parseAlmanach seedRangeListParser input of
        Ok { seeds, seedToSoil, soilToFertilizer, fertilizerToWater, waterToLight, lightToTemperature, temperatureToHumidity, humidityToLocation } ->
            seeds |> recursivelyMapRanges [ seedToSoil, soilToFertilizer, fertilizerToWater, waterToLight, lightToTemperature, temperatureToHumidity, humidityToLocation ]

        Err err ->
            Err err


type alias Almanach seedType =
    { seeds : List seedType
    , seedToSoil : List MappingRange
    , soilToFertilizer : List MappingRange
    , fertilizerToWater : List MappingRange
    , waterToLight : List MappingRange
    , lightToTemperature : List MappingRange
    , temperatureToHumidity : List MappingRange
    , humidityToLocation : List MappingRange
    }


type alias Range =
    { start : Int, end : Int }


type alias MappingRange =
    { destinationStart : Int
    , sourceStart : Int
    , rangeLength : Int
    }


mapDefinitionsParser : Parser (List MappingRange)
mapDefinitionsParser =
    Parser.loop []
        (\definitions ->
            Parser.oneOf
                [ Parser.succeed (\x y z -> MappingRange x y z :: definitions |> Loop)
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


seedRangeListParser : Parser (List Range)
seedRangeListParser =
    Parser.loop []
        (\seeds ->
            Parser.oneOf
                [ Parser.succeed (\start length -> Range start (start + length) :: seeds |> Loop)
                    |. Parser.symbol " "
                    |= Parser.int
                    |. Parser.symbol " "
                    |= Parser.int
                    |> Parser.backtrackable
                , Parser.symbol "\n" |> Parser.map (\_ -> seeds |> List.sortBy .start |> Done) |> Parser.backtrackable
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
