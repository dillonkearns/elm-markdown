module Markdown.TableParser exposing (..)

import Parser
import Parser.Advanced as Advanced exposing (..)
import Parser.Extra exposing (oneOrMore, tokenHelp)


type alias Parser a =
    Advanced.Parser String Parser.Problem a


type alias Row =
    List String


parser : Parser Table
parser =
    Advanced.succeed
        (\header ->
            Table
                header
                []
        )
        |= rowParser
        |. delimiterRowParser


rowParser : Parser (List String)
rowParser =
    succeed
        (\rowString ->
            rowString
                |> dropTrailingPipe
                |> String.split "|"
        )
        |. oneOf
            [ tokenHelp "|"
            , succeed ()
            ]
        |= Advanced.getChompedString
            (Advanced.chompUntilEndOr "\n")
        |. oneOf
            [ Advanced.end (Parser.Expecting "end")
            , chompIf (\c -> c == '\n') (Parser.Expecting "\\n")
            ]


type DelimiterRow
    = DelimiterRow Int


delimiterRowParser : Parser DelimiterRow
delimiterRowParser =
    loop 0 statementsHelp


statementsHelp : Int -> Parser (Step Int DelimiterRow)
statementsHelp found =
    oneOf
        [ succeed identity
            |. tokenHelp "|"
            |= oneOf
                [ hyphens found
                , Advanced.end (Parser.Expecting "end")
                    |> map (\_ -> Done (DelimiterRow found))
                ]
        , hyphens found
        , Advanced.end (Parser.Expecting "end")
            |> map (\_ -> Done (DelimiterRow found))
        ]


hyphens found =
    oneOrMore (\c -> c == '-')
        |> map (\_ -> Loop (found + 1))


dropTrailingPipe : String -> String
dropTrailingPipe string =
    if string |> String.endsWith "|" then
        string
            |> String.dropRight 1

    else
        string


type Table
    = Table Row (List Row)
