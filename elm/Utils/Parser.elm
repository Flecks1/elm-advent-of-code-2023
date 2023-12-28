module Utils.Parser exposing (string)

import Parser exposing (Parser)


string : (Char -> Bool) -> Parser String
string isAcceptedCharacter =
    Parser.chompIf isAcceptedCharacter
        |> Parser.andThen (\_ -> Parser.chompWhile isAcceptedCharacter)
        |> Parser.getChompedString
