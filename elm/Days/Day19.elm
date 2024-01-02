module Days.Day19 exposing (first, second)

import Dict exposing (Dict)
import Dict.Extra as Dict
import List.Extra as List
import Maybe.Extra as Maybe
import Parser exposing ((|.), (|=), Parser)
import Puzzle
import Utils.Parser as Parser
import Utils.Various exposing (iff)


first : Puzzle.Solution
first =
    let
        solve { workflows, parts } =
            parts |> List.filter (isPartAccepted workflows "in") |> List.map (Dict.foldl (\_ -> (+)) 0) |> List.sum |> String.fromInt |> Ok
    in
    parsePuzzleModel >> solve


isPartAccepted : Dict WorkflowId Workflow -> WorkflowId -> Part -> Bool
isPartAccepted workflows workflowId part =
    let
        fullfillsCondition parameter condition =
            Dict.get parameter
                >> Maybe.unwrap False
                    (\partValue ->
                        case condition of
                            GreaterThan n ->
                                partValue > n

                            LowerThan n ->
                                partValue < n

                            _ ->
                                False
                    )

        goThroughRules rules =
            rules |> List.stoppableFoldl (\{ parameter, condition, result } _ -> iff (fullfillsCondition parameter condition part) (Just result |> List.Stop) (List.Continue Nothing)) Nothing
    in
    case Dict.get workflowId workflows of
        Just { rules, defaultResult } ->
            case goThroughRules rules |> Maybe.withDefault defaultResult of
                Accepted ->
                    True

                Refused ->
                    False

                GoTo newWorkflowId ->
                    isPartAccepted workflows newWorkflowId part

        Nothing ->
            False


second : Puzzle.Solution
second =
    parsePuzzleModel >> .workflows >> countAllAcceptableParts >> String.fromInt >> Ok


countAllAcceptableParts : Dict WorkflowId Workflow -> Int
countAllAcceptableParts workflows =
    let
        handleBothPossibilitiesForRule rule ( currentPotentialPart, possibleParts, amountOfAcceptedPartsPossible ) =
            let
                testCondition condition =
                    currentPotentialPart |> Maybe.map (updatePotentialPartConditions rule.parameter condition) |> Maybe.filter isPotentialPartValid

                partThatDoesFullfillRule =
                    testCondition rule.condition

                partThatDoesNotFullfillRule =
                    testCondition (inverseCondition rule.condition)

                stopIfCurrentPotentialPartCannotContinue ( newCurrentPotentialPart, newPossibleParts, newAmountOfAcceptedPartsPossible ) =
                    if newCurrentPotentialPart == Nothing then
                        List.Stop ( newCurrentPotentialPart, newPossibleParts, newAmountOfAcceptedPartsPossible )

                    else
                        List.Continue ( newCurrentPotentialPart, newPossibleParts, newAmountOfAcceptedPartsPossible )
            in
            case rule.result of
                Refused ->
                    stopIfCurrentPotentialPartCannotContinue ( partThatDoesNotFullfillRule, possibleParts, amountOfAcceptedPartsPossible )

                Accepted ->
                    stopIfCurrentPotentialPartCannotContinue
                        ( partThatDoesNotFullfillRule
                        , possibleParts
                        , partThatDoesFullfillRule |> Maybe.unwrap amountOfAcceptedPartsPossible (\part -> amountOfAcceptedPartsPossible + calculateAmountOfRealParts part)
                        )

                GoTo newWorkflow ->
                    stopIfCurrentPotentialPartCannotContinue
                        ( partThatDoesNotFullfillRule
                        , partThatDoesFullfillRule |> Maybe.unwrap possibleParts (\part -> ( newWorkflow, part ) :: possibleParts)
                        , amountOfAcceptedPartsPossible
                        )

        handleAllPosibilitiesForNextWorkflow ( nextWorkflow, part ) ( possibleParts, amountOfAcceptedPartsPossible ) =
            Dict.get nextWorkflow workflows
                |> Maybe.unwrap ( possibleParts, amountOfAcceptedPartsPossible )
                    (\{ rules, defaultResult } ->
                        let
                            ( maybeNewPart, newPossibleParts, newAmountOfAcceptedPartsPossible ) =
                                rules |> List.stoppableFoldl handleBothPossibilitiesForRule ( Just part, possibleParts, amountOfAcceptedPartsPossible )
                        in
                        case ( maybeNewPart, defaultResult ) of
                            ( Just newPart, Accepted ) ->
                                ( newPossibleParts, newAmountOfAcceptedPartsPossible + calculateAmountOfRealParts newPart )

                            ( Just newPart, GoTo newWorkflow ) ->
                                ( ( newWorkflow, newPart ) :: newPossibleParts, newAmountOfAcceptedPartsPossible )

                            _ ->
                                ( newPossibleParts, newAmountOfAcceptedPartsPossible )
                    )

        recursiveHelper possibleParts amountOfAcceptedPartsPossible =
            if not (List.isEmpty possibleParts) then
                let
                    ( newPossibleParts, newAmountOfAcceptedPartsPossible ) =
                        possibleParts |> List.foldl handleAllPosibilitiesForNextWorkflow ( [], amountOfAcceptedPartsPossible )
                in
                recursiveHelper newPossibleParts newAmountOfAcceptedPartsPossible

            else
                amountOfAcceptedPartsPossible
    in
    recursiveHelper [ ( "in", Dict.empty ) ] 0


calculateAmountOfRealParts : PossiblePart -> Int
calculateAmountOfRealParts part =
    let
        getPossibleValuesForParameter parameter =
            Dict.get parameter part
                |> Maybe.unwrap 4000
                    (\condition ->
                        case condition of
                            Between i j ->
                                j - i - 1

                            LowerThan i ->
                                i - 1

                            GreaterThan i ->
                                4000 - i

                            _ ->
                                1
                    )
    in
    [ "x", "m", "a", "s" ] |> List.foldl (getPossibleValuesForParameter >> (*)) 1


updatePotentialPartConditions : String -> Condition -> PossiblePart -> PossiblePart
updatePotentialPartConditions parameter condition =
    let
        updatePartCondition partCondition =
            case ( partCondition, condition ) of
                ( Nothing, _ ) ->
                    Just condition

                ( Just (GreaterThan i), LowerThan j ) ->
                    iff (i + 1 < j) (Between i j) Invalid |> Just

                ( Just (GreaterThan i), GreaterThan j ) ->
                    GreaterThan (max i j) |> Just

                ( Just (LowerThan i), LowerThan j ) ->
                    LowerThan (min i j) |> Just

                ( Just (LowerThan i), GreaterThan j ) ->
                    iff (i - 1 > j) (Between j i) Invalid |> Just

                ( Just (Between i j), LowerThan k ) ->
                    iff (k - 1 > i) (Between i (min j k)) Invalid |> Just

                ( Just (Between i j), GreaterThan k ) ->
                    iff (k + 1 < j) (Between (max i k) j) Invalid |> Just

                _ ->
                    Just Invalid
    in
    Dict.update parameter updatePartCondition


inverseCondition : Condition -> Condition
inverseCondition condition =
    case condition of
        LowerThan n ->
            GreaterThan (n - 1)

        GreaterThan n ->
            LowerThan (n + 1)

        _ ->
            Invalid


isPotentialPartValid : PossiblePart -> Bool
isPotentialPartValid =
    not << Dict.any (\_ -> (==) Invalid)



-- Types


type alias PossiblePart =
    Dict String Condition


type alias PuzzleModel =
    { workflows : Dict WorkflowId Workflow
    , parts : List Part
    }


type alias Workflow =
    { rules : List Rule, defaultResult : WorkflowResult }


type alias Rule =
    { parameter : String, condition : Condition, result : WorkflowResult }


type Condition
    = GreaterThan Int
    | LowerThan Int
    | Between Int Int
    | Invalid


type alias WorkflowId =
    String


type WorkflowResult
    = GoTo WorkflowId
    | Accepted
    | Refused


type alias Part =
    Dict String Int



-- Parsing


ruleParser : Parser Rule
ruleParser =
    Parser.succeed (\parameter condition targetValue result -> Rule parameter (condition targetValue) result)
        |= Parser.string Char.isAlpha
        |= Parser.oneOf
            [ Parser.succeed LowerThan |. Parser.symbol "<"
            , Parser.succeed GreaterThan |. Parser.symbol ">"
            ]
        |= Parser.int
        |. Parser.symbol ":"
        |= (Parser.string Char.isAlpha |> Parser.map parseWorkflowResult)


parseWorkflowResult : String -> WorkflowResult
parseWorkflowResult str =
    case str of
        "A" ->
            Accepted

        "R" ->
            Refused

        _ ->
            GoTo str


parseWorkflows : String -> Dict WorkflowId Workflow
parseWorkflows =
    let
        parseRule =
            Parser.run ruleParser >> Result.toMaybe

        parseWorkflow line workflows =
            case line |> String.dropRight 1 |> String.split "{" of
                [ workflowId, rules ] ->
                    case String.split "," rules |> List.unconsLast of
                        Just ( defaultRule, otherRules ) ->
                            workflows |> Dict.insert workflowId (Workflow (List.filterMap parseRule otherRules) (parseWorkflowResult defaultRule))

                        _ ->
                            workflows

                _ ->
                    workflows
    in
    String.split "\n" >> List.foldl parseWorkflow Dict.empty


parseParts : String -> List Part
parseParts =
    let
        setParameter str dict =
            case String.split "=" str of
                [ parameter, value ] ->
                    dict |> Dict.insert parameter (String.toInt value |> Maybe.withDefault 0)

                _ ->
                    dict

        parsePart =
            String.slice 1 -1 >> String.split "," >> List.foldl setParameter Dict.empty
    in
    String.split "\n" >> List.map parsePart


parsePuzzleModel : String -> PuzzleModel
parsePuzzleModel input =
    case String.split "\n\n" input of
        [ workflows, parts ] ->
            PuzzleModel (parseWorkflows workflows) (parseParts parts)

        _ ->
            PuzzleModel Dict.empty []
