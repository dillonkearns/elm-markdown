module Parser.Extra exposing (oneOrMore, positiveInteger, zeroOrMore)

import Parser
import Parser.Advanced as Parser exposing ((|.), Parser, chompIf, chompWhile, mapChompedString, succeed)


oneOrMore : (Char -> Bool) -> Parser c Parser.Problem ()
oneOrMore condition =
    chompIf condition (Parser.Problem "Expected one or more character")
        |. chompWhile condition


zeroOrMore : (Char -> Bool) -> Parser c x ()
zeroOrMore condition =
    chompWhile condition


positiveInteger : Parser c Parser.Problem Int
positiveInteger =
    succeed ()
        |. oneOrMore Char.isDigit
        |> mapChompedString
            (\str _ -> String.toInt str |> Maybe.withDefault 0)
