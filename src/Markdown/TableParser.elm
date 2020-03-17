module Markdown.TableParser exposing (..)

import Markdown.Table
import Parser
import Parser.Advanced as Advanced exposing (..)
import Parser.Extra exposing (oneOrMore, tokenHelp)


type alias Parser a =
    Advanced.Parser String Parser.Problem a


type alias Table =
    Markdown.Table.Table String


parser : Parser Table
parser =
    Advanced.succeed
        (\header ->
            Markdown.Table.Table
                (header
                    |> List.map
                        (\headerCell ->
                            { label = headerCell
                            , alignment = Nothing
                            }
                        )
                )
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
        |> andThen
            (\((DelimiterRow count) as delimiterRow) ->
                if count > 0 then
                    succeed delimiterRow

                else
                    problem (Parser.Expecting "Must have at least one column in delimiter row.")
            )


statementsHelp : Int -> Parser (Step Int DelimiterRow)
statementsHelp found =
    oneOf
        [ succeed identity
            |. tokenHelp "|"
            |= oneOf
                [ hyphens found
                , Advanced.end (Parser.Expecting "end")
                    |> map (\_ -> Done (DelimiterRow found))
                , tokenHelp "\n"
                    --|. chompIf (\c -> c == '\n') (Parser.Expecting "\\n")
                    |> map (\_ -> Done (DelimiterRow found))
                ]
        , hyphens found
        , tokenHelp "\n"
            --|. chompIf (\c -> c == '\n') (Parser.Expecting "\\n")
            |> map (\_ -> Done (DelimiterRow found))
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
