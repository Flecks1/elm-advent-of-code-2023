module Days.Day7 exposing (first, second)

import List.Extra as List
import Maybe.Extra as Maybe
import Parser exposing ((|.), (|=), Parser, Step(..))
import Puzzle
import Result.Extra as Result
import Set
import Utils.Parser as Parser


first : Puzzle.Solution
first =
    commonHelper False


second : Puzzle.Solution
second =
    commonHelper True


commonHelper : ShouldHandleWildcards -> Puzzle.Solution
commonHelper shouldHandleWildcards =
    parseBids shouldHandleWildcards
        >> Result.map
            (List.sortBy (\{ hand, originalHandCardValues } -> handToRank hand :: originalHandCardValues)
                >> List.indexedFoldl (\i { value } sum -> sum + (i + 1) * value) 0
                >> String.fromInt
            )


type Hand
    = FiveOfAKind CardValue
    | FourOfAKind FourOfAKindCardValue CardValue
    | FullOfHouse ThreeOfAKindCardValue PairCardValue
    | ThreeOfAKind ThreeOfAKindCardValue CardValue CardValue
    | TwoPair PairCardValue PairCardValue CardValue
    | OnePair PairCardValue CardValue CardValue CardValue
    | HighCard CardValue CardValue CardValue CardValue CardValue


wildCardValue : Int
wildCardValue =
    1


toHandCardValues : ShouldHandleWildcards -> String -> Result String (List Int)
toHandCardValues shouldHandleWildcards =
    let
        cardValueFromChar char =
            case char of
                'A' ->
                    Ok 14

                'K' ->
                    Ok 13

                'Q' ->
                    Ok 12

                'J' ->
                    if shouldHandleWildcards then
                        Ok wildCardValue

                    else
                        Ok 11

                'T' ->
                    Ok 10

                '9' ->
                    Ok 9

                '8' ->
                    Ok 8

                '7' ->
                    Ok 7

                '6' ->
                    Ok 6

                '5' ->
                    Ok 5

                '4' ->
                    Ok 4

                '3' ->
                    Ok 3

                '2' ->
                    Ok 2

                _ ->
                    Err ("Invalid card '" ++ String.fromChar char ++ "'")
    in
    String.foldr (cardValueFromChar >> Result.map2 (::)) (Ok [])


handleWildCards : ShouldHandleWildcards -> List (List Int) -> List (List Int)
handleWildCards shouldHandleWildcards cardGroups =
    let
        putWildcardsInBiggestGroup remainingCardGroups passedCardGroups =
            case remainingCardGroups of
                ((cardValue :: _) as currentGroup) :: restOfRemainingCardGroups ->
                    if cardValue == wildCardValue then
                        List.uncons (List.reverse passedCardGroups ++ restOfRemainingCardGroups)
                            |> Maybe.map (\( firstGroup, nextGroups ) -> List.append firstGroup currentGroup :: nextGroups)

                    else
                        putWildcardsInBiggestGroup restOfRemainingCardGroups (currentGroup :: passedCardGroups)

                _ ->
                    Nothing
    in
    if shouldHandleWildcards then
        putWildcardsInBiggestGroup cardGroups []
            |> Maybe.withDefault cardGroups

    else
        cardGroups


handParser : ShouldHandleWildcards -> Parser ( List Int, Hand )
handParser shouldHandleWildcards =
    let
        acceptedChars =
            "AKQJT98765432" |> String.foldl Set.insert Set.empty
    in
    Parser.string (\char -> Set.member char acceptedChars)
        |> Parser.andThen
            -- Transform each card symbol into an Int representing its value
            (toHandCardValues shouldHandleWildcards
                >> Result.andThen
                    (\handCardValues ->
                        handCardValues
                            -- Sort all values to make groups
                            |> sortDescending
                            |> groupDuplicates
                            -- Sort the groups (bigger groups first, then higher card values)
                            |> sortByDescending cardGroupToRank
                            -- If we're handling wildcards, find them, then add all of them to the highest ranking group
                            |> handleWildCards shouldHandleWildcards
                            -- Categorize the groups into their respective hand types
                            |> cardGroupsToHand
                            -- Pair the result with the original card values in their original order
                            |> Result.map (Tuple.pair handCardValues)
                    )
                >> Result.unpack Parser.problem Parser.succeed
            )


cardGroupsToHand : List (List CardValue) -> Result String Hand
cardGroupsToHand cardGroups =
    case cardGroups of
        [ [ cardValue, _, _, _, _ ] ] ->
            FiveOfAKind cardValue |> Ok

        [ [ fourOfAKindCardValue, _, _, _ ], [ cardValue ] ] ->
            FourOfAKind fourOfAKindCardValue cardValue |> Ok

        [ [ threeOfAKindCardValue, _, _ ], [ pairCardValue, _ ] ] ->
            FullOfHouse threeOfAKindCardValue pairCardValue |> Ok

        [ [ threeOfAKindCardValue, _, _ ], [ cardValue1 ], [ cardValue2 ] ] ->
            ThreeOfAKind threeOfAKindCardValue cardValue1 cardValue2 |> Ok

        [ [ pairCardValue1, _ ], [ pairCardValue2, _ ], [ cardValue ] ] ->
            TwoPair pairCardValue1 pairCardValue2 cardValue |> Ok

        [ [ pairCardValue, _ ], [ cardValue1 ], [ cardValue2 ], [ cardValue3 ] ] ->
            OnePair pairCardValue cardValue1 cardValue2 cardValue3 |> Ok

        [ [ cardValue1 ], [ cardValue2 ], [ cardValue3 ], [ cardValue4 ], [ cardValue5 ] ] ->
            HighCard cardValue1 cardValue2 cardValue3 cardValue4 cardValue5 |> Ok

        _ ->
            Err "Invalid hand type"


groupDuplicates : List a -> List (List a)
groupDuplicates list =
    let
        step x ( group, acc ) =
            case group of
                [] ->
                    ( [ x ], acc )

                y :: _ ->
                    if x == y then
                        ( x :: group, acc )

                    else
                        ( [ x ], group :: acc )
    in
    case list of
        [] ->
            []

        x :: xs ->
            List.foldl step ( [ x ], [] ) xs
                |> (\( y, ys ) -> y :: ys)


handToRank : Hand -> Int
handToRank hand =
    case hand of
        FiveOfAKind _ ->
            7

        FourOfAKind _ _ ->
            6

        FullOfHouse _ _ ->
            5

        ThreeOfAKind _ _ _ ->
            4

        TwoPair _ _ _ ->
            3

        OnePair _ _ _ _ ->
            2

        HighCard _ _ _ _ _ ->
            1


cardGroupToRank : List Int -> Int
cardGroupToRank cardGroup =
    case cardGroup of
        [ cardValue, _, _, _, _ ] ->
            500 + cardValue

        [ cardValue, _, _, _ ] ->
            400 + cardValue

        [ cardValue, _, _ ] ->
            300 + cardValue

        [ cardValue, _ ] ->
            200 + cardValue

        [ cardValue ] ->
            100 + cardValue

        _ ->
            0


sortDescending : List comparable -> List comparable
sortDescending =
    sortByDescending identity


sortByDescending : (a -> comparable) -> List a -> List a
sortByDescending toComparable =
    let
        flippedComparison a b =
            case compare (toComparable a) (toComparable b) of
                LT ->
                    GT

                EQ ->
                    EQ

                GT ->
                    LT
    in
    List.sortWith flippedComparison


type alias Bid =
    { value : Int
    , originalHandCardValues : List Int
    , hand : Hand
    }


parseBids : ShouldHandleWildcards -> String -> Result String (List Bid)
parseBids shouldHandleWildcards =
    let
        parser =
            Parser.loop []
                (\bids ->
                    Parser.oneOf
                        [ Parser.succeed (\( originalHandCardValues, hand ) bidValue -> Bid bidValue originalHandCardValues hand :: bids |> Loop)
                            |. Parser.spaces
                            |= handParser shouldHandleWildcards
                            |. Parser.spaces
                            |= Parser.int
                        , Parser.end |> Parser.map (\_ -> Done bids)
                        ]
                )
    in
    Parser.run parser >> Result.mapError (Debug.log "deadEnds" >> (\_ -> "Parser error, look in the console"))



-- Type aliases for clarity


type alias FourOfAKindCardValue =
    Int


type alias ThreeOfAKindCardValue =
    Int


type alias PairCardValue =
    Int


type alias CardValue =
    Int


type alias ShouldHandleWildcards =
    Bool
