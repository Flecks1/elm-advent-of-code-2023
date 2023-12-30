module Utils.Direction exposing (Direction, apply, applyWith, down, inverse, isOppositeTo, left, none, right, up)


type alias Direction =
    ( Int, Int )


down : Direction
down =
    ( 0, 1 )


left : Direction
left =
    ( -1, 0 )


right : Direction
right =
    ( 1, 0 )


up : Direction
up =
    ( 0, -1 )


none : Direction
none =
    ( 0, 0 )


apply : Direction -> Position -> Position
apply =
    applyWith 1


applyWith : Int -> Direction -> Position -> Position
applyWith amount ( deltaX, deltaY ) ( x, y ) =
    ( x + deltaX * amount, y + deltaY * amount )


isOppositeTo : Direction -> Direction -> Bool
isOppositeTo target source =
    source == inverse target


inverse : Direction -> Direction
inverse ( x, y ) =
    ( -x, -y )



-- Private type aliases for clarity


type alias Position =
    ( Int, Int )
