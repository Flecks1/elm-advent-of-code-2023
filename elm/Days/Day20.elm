module Days.Day20 exposing (first, second)

import Dict exposing (Dict)
import Dict.Extra as Dict
import Html exposing (button)
import List.Extra as List
import Maybe.Extra as Maybe
import Puzzle
import String.Extra as String
import Utils.Various exposing (iff)


first : Puzzle.Solution
first =
    let
        incrementLowAndHighPulses { pulseType } ({ lowPulses, highPulses } as state) =
            if pulseType == Low then
                { state | lowPulses = lowPulses + 1 }

            else
                { state | highPulses = highPulses + 1 }

        sendAndCountPulses remainingPulses ({ pulsesToHandle, lowPulses, highPulses } as state) =
            if not (List.isEmpty pulsesToHandle) then
                let
                    handlePulse pulseToHandle =
                        incrementLowAndHighPulses pulseToHandle >> sendPulse pulseToHandle

                    newState =
                        pulsesToHandle |> List.foldl handlePulse { state | pulsesToHandle = [] }
                in
                sendAndCountPulses remainingPulses newState

            else if remainingPulses > 0 then
                sendAndCountPulses (remainingPulses - 1) { state | pulsesToHandle = [ PulseToHandle "button" Low broadcaster ] }

            else
                lowPulses * highPulses
    in
    parseModules >> PulseStatePartOne 0 0 [] >> sendAndCountPulses 1000 >> String.fromInt >> Ok


type alias PulseStatePartOne =
    { lowPulses : Int
    , highPulses : Int
    , pulsesToHandle : List PulseToHandle
    , modules : Modules
    }


second : Puzzle.Solution
second input =
    let
        monitorPulse buttonPresses { sourceModule, pulseType } ({ monitoredPulseCycles } as state) =
            -- Since the conjunctions we monitored are assumed to be inverters with only one input, all we need to remember
            -- is the moment (amount of button presses) when they send a high pulse
            if pulseType == High then
                { state | monitoredPulseCycles = monitoredPulseCycles |> Dict.update sourceModule (Maybe.map (Maybe.orElse (Just buttonPresses))) }

            else
                state

        countButtonPressesUntilLowPulseIsSentToRx buttonPresses ({ pulsesToHandle, monitoredPulseCycles } as state) =
            if not (List.isEmpty pulsesToHandle) then
                let
                    handlePulse pulseToHandle =
                        monitorPulse buttonPresses pulseToHandle >> sendPulse pulseToHandle

                    newState =
                        pulsesToHandle |> List.foldl handlePulse { state | pulsesToHandle = [] }
                in
                countButtonPressesUntilLowPulseIsSentToRx buttonPresses newState

            else if monitoredPulseCycles |> Dict.any (\_ -> (==) Nothing) then
                countButtonPressesUntilLowPulseIsSentToRx (buttonPresses + 1) { state | pulsesToHandle = [ PulseToHandle "button" Low broadcaster ] }

            else
                monitoredPulseCycles |> Dict.foldl (\_ -> Maybe.withDefault 1 >> (*)) 1

        parsedModules =
            parseModules input
    in
    PulseStatePartTwo [] (getInitialMonitoredPulseCycles parsedModules) parsedModules |> countButtonPressesUntilLowPulseIsSentToRx 0 |> String.fromInt |> Ok


getInitialMonitoredPulseCycles : Modules -> Dict ModuleId (Maybe Int)
getInitialMonitoredPulseCycles modules =
    -- We assume rx has a single input, which is a conjunction, which itself only has one or more inputs that are only inverters, which obey a certain cycle.
    -- Whenever these conjunctions will send a high pulse all at once, the single input from rx should then send a single low pulse.
    -- What we want to determine is when does that happen according to the cycles we can find from monitoring these inverters two layers up.
    Dict.find (\_ -> hasOutput rx) modules
        |> Maybe.map (Tuple.first >> getConjunctionInputs modules)
        |> Maybe.withDefault []
        |> List.foldl (\monitoredModuleId -> Dict.insert monitoredModuleId Nothing) Dict.empty


type alias PulseStatePartTwo =
    { pulsesToHandle : List PulseToHandle
    , monitoredPulseCycles : Dict ModuleId (Maybe Int)
    , modules : Modules
    }



-- Main update logic


type alias PulseToHandle =
    { sourceModule : ModuleId
    , pulseType : PulseType
    , targetModule : ModuleId
    }


sendPulse : PulseToHandle -> { model | modules : Modules, pulsesToHandle : List PulseToHandle } -> { model | modules : Modules, pulsesToHandle : List PulseToHandle }
sendPulse { sourceModule, pulseType, targetModule } ({ modules, pulsesToHandle } as state) =
    case Dict.get targetModule modules of
        Just (Broadcaster outputs) ->
            { state | pulsesToHandle = outputs |> List.map (PulseToHandle targetModule pulseType) |> List.append pulsesToHandle }

        Just (FlipFlop isOn outputs) ->
            if pulseType == High then
                state

            else
                { state
                    | modules = modules |> Dict.insert targetModule (FlipFlop (not isOn) outputs)
                    , pulsesToHandle = outputs |> List.map (PulseToHandle targetModule (iff isOn Low High)) |> List.append pulsesToHandle
                }

        Just (Conjunction inputs outputs) ->
            let
                newInputs =
                    inputs |> Dict.insert sourceModule pulseType

                wereAllLastInputsHigh =
                    not <| Dict.any (\_ -> (==) Low) newInputs
            in
            { state
                | modules = modules |> Dict.insert targetModule (Conjunction newInputs outputs)
                , pulsesToHandle = outputs |> List.map (PulseToHandle targetModule (iff wereAllLastInputsHigh Low High)) |> List.append pulsesToHandle
            }

        _ ->
            state



-- Types


broadcaster : ModuleId
broadcaster =
    "broadcaster"


rx : ModuleId
rx =
    "rx"


type alias Modules =
    Dict ModuleId Module


type alias ModuleId =
    String


type Module
    = Broadcaster (List ModuleId)
    | FlipFlop Bool (List ModuleId)
    | Conjunction (Dict ModuleId PulseType) (List ModuleId)


type PulseType
    = High
    | Low



-- Helpers


hasOutput : ModuleId -> Module -> Bool
hasOutput moduleId =
    getOutputs >> List.member moduleId


getOutputs : Module -> List ModuleId
getOutputs module_ =
    case module_ of
        Broadcaster outputs ->
            outputs

        FlipFlop _ outputs ->
            outputs

        Conjunction _ outputs ->
            outputs


getConjunctionInputs : Modules -> ModuleId -> List ModuleId
getConjunctionInputs modules moduleId =
    case Dict.get moduleId modules of
        Just (Conjunction inputs _) ->
            Dict.keys inputs

        _ ->
            []


getInverterInput : Modules -> ModuleId -> Maybe ModuleId
getInverterInput modules moduleId =
    case Dict.get moduleId modules of
        Just (Conjunction inputs _) ->
            case Dict.keys inputs of
                [ input ] ->
                    Just input

                _ ->
                    Nothing

        _ ->
            Nothing



-- Parsing


parseModules : String -> Modules
parseModules input =
    let
        parseModule line ( parsedModules, parsedInputsForModule ) =
            case String.split " -> " line of
                [ moduleIdentifier, outputsString ] ->
                    let
                        outputs =
                            String.split ", " outputsString

                        addInput inputModuleId outputModuleId =
                            Dict.update outputModuleId (Maybe.unwrap (Just [ inputModuleId ]) ((::) inputModuleId >> Just))

                        updateState cleanModuleIdentifier moduleConstructor =
                            ( parsedModules |> Dict.insert cleanModuleIdentifier (moduleConstructor outputs)
                            , outputs |> List.foldl (addInput cleanModuleIdentifier) parsedInputsForModule
                            )
                    in
                    if moduleIdentifier == broadcaster then
                        updateState broadcaster Broadcaster

                    else if String.left 1 moduleIdentifier == "%" then
                        updateState (String.dropLeft 1 moduleIdentifier) (FlipFlop False)

                    else if String.left 1 moduleIdentifier == "&" then
                        updateState (String.dropLeft 1 moduleIdentifier) (Conjunction Dict.empty)

                    else
                        ( parsedModules, parsedInputsForModule )

                _ ->
                    ( parsedModules, parsedInputsForModule )

        ( modules, inputsForModule ) =
            String.split "\n" input |> List.foldl parseModule ( Dict.empty, Dict.empty )

        getConjunctionDefaultInputs moduleId =
            Dict.get moduleId inputsForModule
                |> Maybe.withDefault []
                |> List.foldl (\inputModule -> Dict.insert inputModule Low) Dict.empty

        addInputsToConjunction moduleId module_ =
            case module_ of
                Conjunction _ outputs ->
                    Conjunction (getConjunctionDefaultInputs moduleId) outputs

                _ ->
                    module_
    in
    modules |> Dict.map addInputsToConjunction
