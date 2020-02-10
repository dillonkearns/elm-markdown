module Markdown.OrderedList exposing (parser)

import Helpers
import Markdown.RawBlock exposing (RawBlock(..))
import Parser
import Parser.Advanced as Advanced exposing (..)
import Parser.Extra exposing (oneOrMore)


type alias Parser a =
    Advanced.Parser String Parser.Problem a


type alias ListItem =
    String


parser : Maybe RawBlock -> Parser ( Int, List ListItem )
parser lastBlock =
    openingItemParser lastBlock
        |> andThen
            (\( startingIndex, listMarker, firstItem ) ->
                loop [] (statementsHelp listMarker firstItem)
                    |> map (\items -> ( startingIndex, items ))
            )


positiveIntegerMaxOf9Digits : Parser Int
positiveIntegerMaxOf9Digits =
    Parser.Extra.positiveInteger
        |> Advanced.andThen
            (\parsed ->
                if parsed <= 999999999 then
                    Advanced.succeed parsed

                else
                    Advanced.problem (Parser.Problem "Starting numbers must be nine digits or less.")
            )


listMarkerParser : Parser ( Int, String )
listMarkerParser =
    let
        markerOption : String -> Parser String
        markerOption marker =
            Advanced.getChompedString (Advanced.symbol (Advanced.Token marker (Parser.ExpectingSymbol marker)))
    in
    succeed Tuple.pair
        |= positiveIntegerMaxOf9Digits
        |= Advanced.oneOf
            [ markerOption "."
            , markerOption ")"
            ]


openingItemParser : Maybe RawBlock -> Parser ( Int, String, ListItem )
openingItemParser lastBlock =
    let
        validateStartsWith1 parsed =
            case parsed of
                ( 1, _ ) ->
                    Advanced.succeed parsed

                _ ->
                    Advanced.problem (Parser.Problem "Lists inside a paragraph or after a paragraph without a blank line must start with 1")

        validateStartsWith1IfInParagraph parsed =
            case lastBlock of
                Just (Body _) ->
                    validateStartsWith1 parsed

                _ ->
                    succeed parsed
    in
    succeed (\( startingIndex, marker ) item -> ( startingIndex, marker, item ))
        |= (backtrackable (listMarkerParser |> andThen validateStartsWith1IfInParagraph)
                |. oneOrMore Helpers.isSpacebar
           )
        |= Advanced.getChompedString (Advanced.chompUntilEndOr "\n")
        |. Advanced.symbol (Advanced.Token "\n" (Parser.ExpectingSymbol "\n"))


singleItemParser : String -> Parser ListItem
singleItemParser listMarker =
    succeed identity
        |. backtrackable
            (Parser.Extra.positiveInteger
                |. Advanced.symbol (Advanced.Token listMarker (Parser.ExpectingSymbol listMarker))
            )
        |= itemBody


itemBody : Parser ListItem
itemBody =
    oneOf
        [ succeed identity
            |. backtrackable (oneOrMore Helpers.isSpacebar)
            |. commit ""
            |= Advanced.getChompedString (Advanced.chompUntilEndOr "\n")
            |. oneOf
                [ Advanced.symbol (Advanced.Token "\n" (Parser.ExpectingSymbol "\\n"))
                , Advanced.end (Parser.Expecting "End of input")
                ]
        , succeed ""
            |. Advanced.symbol (Advanced.Token "\n" (Parser.ExpectingSymbol "\\n"))
        ]


statementsHelp : String -> ListItem -> List ListItem -> Parser (Step (List ListItem) (List ListItem))
statementsHelp listMarker firstItem revStmts =
    oneOf
        [ succeed
            (\offsetBefore stmt offsetAfter ->
                Loop (stmt :: revStmts)
            )
            |= Advanced.getOffset
            |= singleItemParser listMarker
            |= Advanced.getOffset
        , succeed ()
            |> map (\_ -> Done (firstItem :: List.reverse revStmts))
        ]
