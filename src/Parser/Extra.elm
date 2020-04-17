module Parser.Extra exposing (oneOrMore, positiveInteger, tokenHelp, zeroOrMore)

import Parser
import Parser.Advanced as Advanced exposing ((|.), Parser, chompIf, chompWhile, mapChompedString, succeed)


oneOrMore : (Char -> Bool) -> Parser c Parser.Problem ()
oneOrMore condition =
    chompIf condition (Parser.Problem "Expected one or more character")
        |. chompWhile condition


zeroOrMore : (Char -> Bool) -> Parser c x ()
zeroOrMore condition =
    chompWhile condition


positiveInteger : Parser c Parser.Problem Int
positiveInteger =
    mapChompedString (\str _ -> String.toInt str |> Maybe.withDefault 0) <|
        oneOrMore Char.isDigit


tokenHelp : String -> Parser c Parser.Problem ()
tokenHelp char =
    Advanced.token (Advanced.Token char (Parser.Expecting char))
