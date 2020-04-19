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
                loop [] (statementsHelp listMarker)
                    |> map (\items -> ( startingIndex, firstItem :: items ))
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


markerOption : String -> Parser String
markerOption marker =
    succeed marker
        |. Advanced.symbol (Advanced.Token marker (Parser.ExpectingSymbol marker))


openingItemParser : Maybe RawBlock -> Parser ( Int, String, ListItem )
openingItemParser lastBlock =
    succeed (\startingIndex marker item -> ( startingIndex, marker, item ))
        |= backtrackable
            (case lastBlock of
                Just (Body _) ->
                    positiveIntegerMaxOf9Digits |> andThen validateStartsWith1

                _ ->
                    positiveIntegerMaxOf9Digits
            )
        |= backtrackable
            (Advanced.oneOf
                [ markerOption "."
                , markerOption ")"
                ]
            )
        |. oneOrMore Helpers.isSpacebar
        |= Advanced.getChompedString (Advanced.chompUntilEndOr "\n")
        |. Advanced.symbol (Advanced.Token "\n" (Parser.ExpectingSymbol "\n"))


{-| Lists inside a paragraph, or after a paragraph without a line break, must start with index 1.
-}
validateStartsWith1 : Int -> Parser Int
validateStartsWith1 parsed =
    case parsed of
        1 ->
            Advanced.succeed parsed

        _ ->
            Advanced.problem (Parser.Problem "Lists inside a paragraph or after a paragraph without a blank line must start with 1")


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
            |. oneOrMore Helpers.isSpacebar
            |= Advanced.getChompedString (Advanced.chompUntilEndOr "\n")
            |. oneOf
                [ Advanced.symbol (Advanced.Token "\n" (Parser.ExpectingSymbol "\\n"))
                , Advanced.end (Parser.Expecting "End of input")
                ]
        , succeed ""
            |. Advanced.symbol (Advanced.Token "\n" (Parser.ExpectingSymbol "\\n"))
        ]


statementsHelp : String -> List ListItem -> Parser (Step (List ListItem) (List ListItem))
statementsHelp listMarker revStmts =
    oneOf
        [ succeed (\stmt -> Loop (stmt :: revStmts))
            |= singleItemParser listMarker
        , succeed (Done (List.reverse revStmts))
        ]
