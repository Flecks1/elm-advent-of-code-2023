module Utils.Various exposing (iff)


iff : Bool -> a -> a -> a
iff pred ifTrue ifFalse =
    if pred then
        ifTrue

    else
        ifFalse
