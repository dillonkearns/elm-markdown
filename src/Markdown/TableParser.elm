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
                    Advanced.problem (Parser.Problem ("Tables must have the same number of header columns (" ++ String.fromInt (List.length headers) ++ ") as delimiter columns (" ++ String.fromInt (List.length columnAlignments) ++ ")"))
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
        (\revCells ->
            revCells
                |> List.foldl (\cell acc -> formatCell cell :: acc) []
        )
        |. oneOf
            [ tokenHelp "|"
            , succeed ()
            ]
        |= parseCells


formatCell : String -> String
formatCell =
    String.replace "\\|" "|"
        >> String.trim


parseCells : Parser (List String)
parseCells =
    loop ( Nothing, [] ) parseCellHelper


parseCellHelper : ( Maybe String, List String ) -> Parser (Step ( Maybe String, List String ) (List String))
parseCellHelper ( curr, acc ) =
    let
        addToCurrent c =
            c ++ Maybe.withDefault "" curr

        continueCell c =
            Loop ( Just (addToCurrent c), acc )

        finishCell =
            curr
                |> Maybe.map (\cell -> Loop ( Nothing, String.reverse cell :: acc ))
                |> Maybe.withDefault (Loop ( Nothing, acc ))

        return =
            curr
                |> Maybe.map (\cell -> Done (String.reverse cell :: acc))
                |> Maybe.withDefault (Done acc)
    in
    oneOf
        [ tokenHelp "|\n" |> Advanced.map (\_ -> return)
        , tokenHelp "\n" |> Advanced.map (\_ -> return)
        , Advanced.end (Parser.Expecting "end") |> Advanced.map (\_ -> return)
        , backtrackable (succeed (continueCell "|\\")) |. tokenHelp "\\|"
        , backtrackable (succeed finishCell) |. tokenHelp "|"
        , mapChompedString (\char _ -> continueCell char) (chompIf (always True) (Parser.Problem "No character found"))
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


requirePipeIfNotFirst : List a -> Parser ()
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
