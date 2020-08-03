module Parser.Extra exposing (maybeChomp, oneOrMore, positiveInteger, tokenHelp, zeroOrMore)

import Parser
import Parser.Advanced as Advanced exposing ((|.), Parser, chompIf, chompWhile, mapChompedString, oneOf, succeed)


oneOrMore : (Char -> Bool) -> Parser c Parser.Problem ()
oneOrMore condition =
    chompIf condition (Parser.Problem "Expected one or more character")
        |. chompWhile condition


zeroOrMore : (Char -> Bool) -> Parser c x ()
zeroOrMore condition =
    chompWhile condition


maybeChomp : (Char -> Bool) -> Parser c Parser.Problem ()
maybeChomp condition =
    oneOf
        [ chompIf condition (Parser.Problem "Character not found")
        , succeed ()
        ]


positiveInteger : Parser c Parser.Problem Int
positiveInteger =
    succeed ()
        |. oneOrMore Char.isDigit
        |> mapChompedString
            (\str _ -> String.toInt str |> Maybe.withDefault 0)


tokenHelp : String -> Parser c Parser.Problem ()
tokenHelp char =
    Advanced.token (Advanced.Token char (Parser.Expecting char))
