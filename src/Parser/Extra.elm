module Parser.Extra exposing (oneOrMore, zeroOrMore)

import Parser
import Parser.Advanced as Parser exposing ((|.), Parser, chompIf, chompWhile)


oneOrMore : (Char -> Bool) -> Parser c Parser.Problem ()
oneOrMore condition =
    chompIf condition (Parser.Problem "Expected one or more character")
        |. chompWhile condition


zeroOrMore : (Char -> Bool) -> Parser c x ()
zeroOrMore condition =
    chompWhile condition
