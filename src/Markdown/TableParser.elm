module Markdown.TableParser exposing (..)

import Whitespace
import Helpers
import Markdown.Block exposing (Alignment(..))
import Markdown.Table exposing (TableDelimiterRow(..))
import Parser
import Parser.Advanced as Advanced exposing (..)
import Parser.Extra exposing (maybeChomp, chompOneOrMore)
import Parser.Token as Token


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

        validateHeaderMatchesDelimiter ( headers, TableDelimiterRow _ columnAlignments ) =
            if List.length headers == List.length columnAlignments then
                Advanced.succeed (Markdown.Table.TableHeader (headersWithAlignment headers columnAlignments))

            else
                Advanced.problem (Parser.Problem ("Tables must have the same number of header columns (" ++ String.fromInt (List.length headers) ++ ") as delimiter columns (" ++ String.fromInt (List.length columnAlignments) ++ ")"))
    in
    (succeed Tuple.pair
        |= rowParser
        |= delimiterRowParser
    )
        |> andThen validateHeaderMatchesDelimiter


parseHeader : TableDelimiterRow -> String -> Result String (Markdown.Table.TableHeader String)
parseHeader (TableDelimiterRow _ columnAlignments) headersRow =
    let
        headersWithAlignment headers =
            List.map2
                (\headerCell alignment ->
                    { label = headerCell
                    , alignment = alignment
                    }
                )
                headers
                columnAlignments

        combineHeaderAndDelimiter headers =
            if List.length headers == List.length columnAlignments then
                Ok (Markdown.Table.TableHeader (headersWithAlignment headers))

            else
                Err ("Tables must have the same number of header columns (" ++ String.fromInt (List.length headers) ++ ") as delimiter columns (" ++ String.fromInt (List.length columnAlignments) ++ ")")
    in
    case Advanced.run rowParser headersRow of
        Ok headers ->
            combineHeaderAndDelimiter headers

        Err _ ->
            Err "Unable to parse previous line as a table header"


rowParser : Parser (List String)
rowParser =
    succeed identity
        |. oneOf
            [ Token.parseString "|"
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
            Maybe.withDefault "" curr ++ c

        continueCell c =
            Loop ( Just (addToCurrent c), acc )

        finishCell =
            curr
                |> Maybe.map (\cell -> Loop ( Nothing, cell :: acc ))
                |> Maybe.withDefault (Loop ( Nothing, acc ))

        return =
            curr
                |> Maybe.map (\cell -> Done (cell :: acc))
                |> Maybe.withDefault (Done acc)
    in
    oneOf
        [ Token.parseString "|\n" |> Advanced.map (\_ -> return)
        , Token.parseString "\n" |> Advanced.map (\_ -> return)
        , Advanced.end (Parser.Expecting "end") |> Advanced.map (\_ -> return)
        , backtrackable (succeed (continueCell "|")) |. Token.parseString "\\\\|"
        , backtrackable (succeed (continueCell "\\")) |. Token.parseString "\\\\"
        , backtrackable (succeed (continueCell "|")) |. Token.parseString "\\|"
        , backtrackable (succeed finishCell) |. Token.parseString "|"
        , mapChompedString (\char _ -> continueCell char) (chompIf (always True) (Parser.Problem "No character found"))
        ]


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


delimiterRowParser : Parser TableDelimiterRow
delimiterRowParser =
    mapChompedString
        (\delimiterText revDelimiterColumns -> TableDelimiterRow { raw = delimiterText, trimmed = String.trim delimiterText } (List.map delimiterToAlignment (List.reverse revDelimiterColumns)))
        (loop [] delimiterRowHelp)
        |> andThen
            (\((TableDelimiterRow { trimmed } headers) as delimiterRow) ->
                if List.isEmpty headers then
                    problem (Parser.Expecting "Must have at least one column in delimiter row.")

                else if List.length headers == 1 && not (String.startsWith "|" trimmed && String.endsWith "|" trimmed) then
                    problem (Parser.Problem "Tables with a single column must have pipes at the start and end of the delimiter row to avoid ambiguity.")

                else
                    succeed delimiterRow
            )


requirePipeIfNotFirst : List a -> Parser ()
requirePipeIfNotFirst columns =
    if List.isEmpty columns then
        oneOf
            [ Token.parseString "|"
            , succeed ()
            ]

    else
        Token.parseString "|"


delimiterRowHelp : List String -> Parser (Step (List String) (List String))
delimiterRowHelp revDelimiterColumns =
    oneOf
        [ backtrackable (Token.parseString "|\n" |> Advanced.map (\_ -> Done revDelimiterColumns))
        , Token.parseString "\n" |> Advanced.map (\_ -> Done revDelimiterColumns)
        , Advanced.end (Parser.Expecting "end") |> Advanced.map (\_ -> Done revDelimiterColumns)
        , backtrackable (succeed (Done revDelimiterColumns) |. Token.parseString "|" |. Advanced.end (Parser.Expecting "end"))
        , succeed (\column -> Loop (column :: revDelimiterColumns))
            |. requirePipeIfNotFirst revDelimiterColumns
            |. chompSinglelineWhitespace
            |= Advanced.getChompedString
                (succeed ()
                    |. maybeChomp (\c -> c == ':')
                    |. chompOneOrMore (\c -> c == '-')
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
    chompWhile Whitespace.isSpaceOrTab


bodyParser : Int -> Parser (List (List String))
bodyParser expectedRowLength =
    loop [] (bodyParserHelp expectedRowLength)


bodyParserHelp : Int -> List (List String) -> Parser (Step (List (List String)) (List (List String)))
bodyParserHelp expectedRowLength revRows =
    oneOf
        [ Helpers.lineEndOrEnd
            |> map (\_ -> Done (List.reverse revRows))
        , bodyRowParser expectedRowLength
            |> map (\row -> Loop (row :: revRows))
        ]
