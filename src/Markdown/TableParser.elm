module Markdown.TableParser exposing (..)

import Markdown.Block exposing (Alignment(..))
import Markdown.Table
import Parser
import Parser.Advanced as Advanced exposing (..)
import Parser.Extra exposing (maybeChomp, oneOrMore, tokenHelp)


type alias Parser a =
    Advanced.Parser String Parser.Problem a


type alias Table =
    Markdown.Table.Table String


parser : Parser Table
parser =
    (Advanced.succeed (\headers delimiters body -> ( headers, delimiters, body ))
        |= rowParser
        |= delimiterRowParser
        |= bodyParser
    )
        |> Advanced.andThen
            (\( headers, DelimiterRow _ columnAlignments, body ) ->
                if List.length headers == List.length columnAlignments then
                    Advanced.succeed
                        (Markdown.Table.Table
                            (List.map2
                                (\headerCell alignment ->
                                    { label = headerCell
                                    , alignment = alignment
                                    }
                                )
                                headers
                                columnAlignments
                            )
                            (standardizeRowLength (List.length headers) body)
                        )

                else
                    Advanced.problem (Parser.Problem "Tables must have the same number of header columns as delimiter columns")
            )


standardizeRowLength : Int -> List (List String) -> List (List String)
standardizeRowLength expectedLength =
    List.map
        (\row ->
            let
                rowLength =
                    List.length row
            in
            case compare expectedLength rowLength of
                LT ->
                    List.take expectedLength row

                EQ ->
                    row

                GT ->
                    row ++ List.repeat (expectedLength - rowLength) ""
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
    = DelimiterRow String (List (Maybe Alignment))


delimiterToAlignment : String -> Maybe Alignment
delimiterToAlignment cell =
    case ( String.startsWith ":" cell, String.endsWith ":" cell ) of
        ( True, True ) ->
            Just AlignCenter

        ( True, False ) ->
            Just AlignLeft

        ( False, True ) ->
            Just AlignRight

        ( False, False ) ->
            Nothing


delimiterRowParser : Parser DelimiterRow
delimiterRowParser =
    mapChompedString
        (\delimiterText revDelimiterColumns -> DelimiterRow (String.trim delimiterText) (List.map delimiterToAlignment (List.reverse revDelimiterColumns)))
        (loop [] delimiterRowHelp)
        |> andThen
            (\((DelimiterRow delimiterText headers) as delimiterRow) ->
                if List.isEmpty headers then
                    problem (Parser.Expecting "Must have at least one column in delimiter row.")

                else if List.length headers == 1 && not (String.startsWith "|" delimiterText && String.endsWith "|" delimiterText) then
                    problem (Parser.Problem "Tables with a single column must have pipes at the start and end of the delimiter row to avoid ambiguity.")

                else
                    succeed delimiterRow
            )


requirePipeIfNotFirst : List String -> Parser ()
requirePipeIfNotFirst columns =
    if List.isEmpty columns then
        oneOf
            [ tokenHelp "|"
            , succeed ()
            ]

    else
        tokenHelp "|"


delimiterRowHelp : List String -> Parser (Step (List String) (List String))
delimiterRowHelp revDelimiterColumns =
    oneOf
        [ backtrackable (tokenHelp "|\n" |> Advanced.map (\_ -> Done revDelimiterColumns))
        , tokenHelp "\n" |> Advanced.map (\_ -> Done revDelimiterColumns)
        , Advanced.end (Parser.Expecting "end") |> Advanced.map (\_ -> Done revDelimiterColumns)
        , backtrackable (succeed (Done revDelimiterColumns) |. tokenHelp "|" |. Advanced.end (Parser.Expecting "end"))
        , succeed (\column -> Loop (column :: revDelimiterColumns))
            |. requirePipeIfNotFirst revDelimiterColumns
            |. chompSpaceCharacter
            |= Advanced.getChompedString
                (succeed ()
                    |. maybeChomp (\c -> c == ':')
                    |. oneOrMore (\c -> c == '-')
                    |. maybeChomp (\c -> c == ':')
                )
            |. chompSpaceCharacter
        ]


chompSpaceCharacter : Parser ()
chompSpaceCharacter =
    chompWhile (\c -> c == ' ')


bodyParser : Parser (List (List String))
bodyParser =
    loop [] bodyParserHelp


bodyParserHelp : List (List String) -> Parser (Step (List (List String)) (List (List String)))
bodyParserHelp revRows =
    oneOf
        [ tokenHelp "\n"
            |> map (\_ -> Done (List.reverse revRows))
        , Advanced.end (Parser.Expecting "end")
            |> map (\_ -> Done (List.reverse revRows))
        , rowParser
            |> andThen
                (\row ->
                    if List.isEmpty row || List.all String.isEmpty row then
                        Advanced.problem (Parser.Problem "A line must have at least one column")

                    else
                        Advanced.succeed (Loop (row :: revRows))
                )
        ]


dropTrailingPipe : String -> String
dropTrailingPipe string =
    if string |> String.endsWith "|" then
        string
            |> String.dropRight 1

    else
        string
