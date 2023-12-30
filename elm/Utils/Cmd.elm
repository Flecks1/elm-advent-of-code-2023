module Utils.Cmd exposing (sendBackMsg)

import Process
import Task


sendBackMsg : Float -> msg -> Cmd msg
sendBackMsg delay msg =
    Process.sleep delay |> Task.perform (\_ -> msg)
