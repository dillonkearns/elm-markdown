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
    (Advanced.succeed Tuple.pair
        |= rowParser
        |= delimiterRowParser
    )
        |> Advanced.andThen
            (\( headers, DelimiterRow delimiterCount ) ->
                if List.length headers == delimiterCount then
                    Advanced.succeed
                        (Markdown.Table.Table
                            (headers
                                |> List.map
                                    (\headerCell ->
                                        { label = headerCell
                                        , alignment = Nothing
                                        }
                                    )
                            )
                            []
                        )

                else
                    Advanced.problem (Parser.Problem "Tables must have the same number of header columns as delimiter columns")
            )


rowParser : Parser (List String)
rowParser =
    succeed
        (\rowString ->
            rowString
                |> dropTrailingPipe
                |> String.split "|"
                |> List.map String.trim
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
    loop 0 delimiterRowHelp
        |> andThen
            (\((DelimiterRow count) as delimiterRow) ->
                if count > 0 then
                    succeed delimiterRow

                else
                    problem (Parser.Expecting "Must have at least one column in delimiter row.")
            )


delimiterRowHelp : Int -> Parser (Step Int DelimiterRow)
delimiterRowHelp found =
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
