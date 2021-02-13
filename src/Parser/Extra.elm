module Parser.Extra exposing (positiveInteger, chompOneOrMore, maybeChomp, upToThree, upTo)

import Parser
import Parser.Advanced as Advanced exposing ((|.), Parser, chompIf, chompWhile, mapChompedString, oneOf, succeed)


type alias Parser a =
    Advanced.Parser String Parser.Problem a


-- Validators


positiveInteger : Parser Int
positiveInteger =
    mapChompedString (\str _ -> String.toInt str |> Maybe.withDefault 0) <|
        chompOneOrMore Char.isDigit


-- Chomp N


chompOneOrMore : (Char -> Bool) -> Parser ()
chompOneOrMore condition =
    chompIf condition (Parser.Problem "Expected one or more character")
        |. chompWhile condition


maybeChomp : (Char -> Bool) -> Parser ()
maybeChomp condition =
    oneOf
        [ chompIf condition (Parser.Problem "Character not found")
        , succeed ()
        ]


-- Parse N


upToThree : Parser () -> Parser ()
upToThree parser =
    oneOf
        [ parser
            |. oneOf [ parser, succeed () ]
            |. oneOf [ parser, succeed () ]
        , succeed ()
        ]


upTo : Int -> Parser () -> Parser ()
upTo n parser =
    case List.repeat n parser of
        -- chompUpTo 0, so nothing to chomp
        [] ->
            succeed ()

        -- Nests lots of oneOf [parser, succeed].
        -- Based on upToThree. If any of the oneOfs
        -- fail, the whole chain is cut short. This
        -- could be replaced with an implementation
        -- that doesn't preload the list with parsers,
        -- as we already have it as an argument.
        firstParser :: remainingParsers ->
            List.foldl
                (\p parsers -> oneOf [ p |. parsers, succeed () ])
                (oneOf [ firstParser, succeed () ])
                remainingParsers