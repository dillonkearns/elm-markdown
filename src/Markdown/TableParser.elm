module Markdown.TableParser exposing (..)

import Helpers
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
    headerParser
        |> andThen
            (\(Markdown.Table.TableHeader headers) ->
                Advanced.succeed (\body -> Markdown.Table.Table headers body)
                    |= bodyParser (List.length headers)
            )


headerParser : Parser (Markdown.Table.TableHeader String)
headerParser =
    let
        headersWithAlignment headers columnAlignments =
            List.map2
                (\headerCell alignment ->
                    { label = headerCell
                    , alignment = alignment
                    }
                )
                headers
                columnAlignments

        validateHeader ( headers, DelimiterRow _ columnAlignments ) =
            if List.length headers == List.length columnAlignments then
                Advanced.succeed (Markdown.Table.TableHeader (headersWithAlignment headers columnAlignments))

            else
                Advanced.problem (Parser.Problem ("Tables must have the same number of header columns (" ++ String.fromInt (List.length headers) ++ ") as delimiter columns (" ++ String.fromInt (List.length columnAlignments) ++ ")"))
    in
    (succeed Tuple.pair
        |= rowParser
        |= delimiterRowParser
    )
        |> andThen validateHeader


rowParser : Parser (List String)
rowParser =
    succeed identity
        |. oneOf
            [ tokenHelp "|"
            , succeed ()
            ]
        |= parseCells


parseCells : Parser (List String)
parseCells =
    loop ( Nothing, [] ) parseCellHelper
        |> Advanced.map (List.foldl (\cell acc -> String.trim cell :: acc) [])


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
        , backtrackable (succeed (continueCell "|")) |. tokenHelp "\\\\|"
        , backtrackable (succeed (continueCell "\\")) |. tokenHelp "\\\\"
        , backtrackable (succeed (continueCell "|")) |. tokenHelp "\\|"
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
            |. chompSinglelineWhitespace
            |= Advanced.getChompedString
                (succeed ()
                    |. maybeChomp (\c -> c == ':')
                    |. oneOrMore (\c -> c == '-')
                    |. maybeChomp (\c -> c == ':')
                )
            |. chompSinglelineWhitespace
        ]


bodyRowParser : Int -> Parser (List String)
bodyRowParser expectedRowLength =
    rowParser
        |> andThen
            (\row ->
                if List.isEmpty row || List.all String.isEmpty row then
                    Advanced.problem (Parser.Problem "A line must have at least one column")

                else
                    Advanced.succeed (standardizeRowLength expectedRowLength row)
            )


standardizeRowLength : Int -> List String -> List String
standardizeRowLength expectedLength row =
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


chompSinglelineWhitespace : Parser ()
chompSinglelineWhitespace =
    chompWhile Helpers.isSpaceOrTab


bodyParser : Int -> Parser (List (List String))
bodyParser expectedRowLength =
    loop [] (bodyParserHelp expectedRowLength)


bodyParserHelp : Int -> List (List String) -> Parser (Step (List (List String)) (List (List String)))
bodyParserHelp expectedRowLength revRows =
    oneOf
        [ tokenHelp "\n"
            |> map (\_ -> Done (List.reverse revRows))
        , Advanced.end (Parser.Expecting "end")
            |> map (\_ -> Done (List.reverse revRows))
        , bodyRowParser expectedRowLength
            |> map (\row -> Loop (row :: revRows))
        ]
