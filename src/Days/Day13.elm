module Days.Day13 exposing (first, second)

import Array exposing (Array)
import Dict exposing (Dict)
import List.Extra as List
import Maybe.Extra as Maybe
import Parser exposing ((|.), (|=), Parser, Step(..))
import Puzzle
import Utils.Parser as Parser


first : Puzzle.Solution
first =
    puzzleHelper False


second : Puzzle.Solution
second =
    puzzleHelper True


puzzleHelper : Bool -> Puzzle.Solution
puzzleHelper shouldHandleSmudges =
    parsePatterns >> Result.map (List.map (getMirroredRowsAndColumns shouldHandleSmudges) >> List.sum >> String.fromInt)


getMirroredRowsAndColumns : Bool -> Pattern -> Int
getMirroredRowsAndColumns shouldHandleSmudges { rows, columns } =
    let
        isPerfectMirroringLine lines maxIndex ( leftIndex, rightIndex, didNotFixASmudgeYet ) =
            let
                ( leftLine, rightLine ) =
                    ( Array.get leftIndex lines, Array.get rightIndex lines )
            in
            if leftIndex < 0 || rightIndex > maxIndex then
                -- If we reached the sides of the pattern, we have a perfect mirroring line
                -- But we return the opposite of didNotFixASmudgeYet, just in case we HAVEN'T fix a smudge, in which case we don't care about this mirroring line
                -- This does not affect the first part since this boolean is False by default in the first part
                not didNotFixASmudgeYet

            else if linesAreEqual leftLine rightLine then
                -- If both lines are equal, check if the remaning lines are also equal
                isPerfectMirroringLine lines maxIndex ( leftIndex - 1, rightIndex + 1, didNotFixASmudgeYet )

            else if didNotFixASmudgeYet && linesWouldBeEqualIfSmudgeWasRemoved leftLine rightLine then
                isPerfectMirroringLine lines maxIndex ( leftIndex - 1, rightIndex + 1, False )

            else
                False

        maybeLinesAreEqual equalityFunction maybeLine1 maybeLine2 =
            Maybe.map2 equalityFunction maybeLine1 maybeLine2 |> Maybe.withDefault False

        linesAreEqual =
            maybeLinesAreEqual (==)

        linesWouldBeEqualIfSmudgeWasRemoved =
            maybeLinesAreEqual
                (\line1 line2 ->
                    List.map2 (==) (String.toList line1) (String.toList line2)
                        |> List.filter not
                        |> (==) [ False ]
                )

        getMirroringLine lines index maxIndex mirroringLines =
            let
                ( leftLine, rightLine ) =
                    ( Array.get index lines, Array.get (index + 1) lines )
            in
            if index + 1 <= maxIndex then
                if linesAreEqual leftLine rightLine then
                    getMirroringLine lines (index + 1) maxIndex (( index, index + 1, shouldHandleSmudges ) :: mirroringLines)

                else if shouldHandleSmudges && linesWouldBeEqualIfSmudgeWasRemoved leftLine rightLine then
                    getMirroringLine lines (index + 1) maxIndex (( index, index + 1, shouldHandleSmudges ) :: mirroringLines)

                else
                    getMirroringLine lines (index + 1) maxIndex mirroringLines

            else
                mirroringLines
                    |> List.filter (isPerfectMirroringLine lines maxIndex)
                    |> List.head
                    |> Maybe.map
                        (\( firstLineIndex, _, _ ) ->
                            firstLineIndex + 1
                        )
    in
    getMirroringLine rows 0 (Array.length rows - 1) []
        |> Maybe.map ((*) 100)
        |> Maybe.orElseLazy (\_ -> getMirroringLine columns 0 (Array.length columns - 1) [])
        |> Maybe.withDefault 0


type alias Pattern =
    { rows : Array String
    , columns : Array String
    }


patternParser : Parser Pattern
patternParser =
    let
        rowsParser =
            Parser.loop []
                (\rows ->
                    Parser.oneOf
                        [ Parser.succeed (\row -> row :: rows |> Loop)
                            |= Parser.string (\c -> c == '.' || c == '#')
                            |. Parser.oneOf [ Parser.symbol "\n", Parser.end ]
                            |> Parser.backtrackable
                        , if List.length rows == 0 then
                            Parser.problem "Expected rows in pattern"

                          else
                            Parser.succeed (List.reverse rows |> Done)
                        ]
                )

        fillColumns line columns =
            String.toList line
                |> List.map2 (\column symbol -> String.fromChar symbol ++ column) columns

        toPattern rows =
            let
                rowSize =
                    List.head rows |> Maybe.unwrap 0 String.length
            in
            { rows = Array.fromList rows
            , columns = rows |> List.foldr fillColumns (List.repeat rowSize "") |> Array.fromList
            }
    in
    rowsParser |> Parser.map toPattern


parsePatterns : String -> Result String (List Pattern)
parsePatterns =
    let
        parser =
            Parser.loop []
                (\patterns ->
                    Parser.oneOf
                        [ Parser.succeed (\pattern -> pattern :: patterns |> Loop)
                            |. Parser.spaces
                            |= patternParser
                            |> Parser.backtrackable
                        , Parser.succeed (List.reverse patterns |> Done)
                            |. Parser.end
                            |> Parser.backtrackable
                        ]
                )
    in
    Parser.run parser >> Result.mapError (Debug.log "deadEnds" >> (\_ -> "Parser error, look in the console"))
