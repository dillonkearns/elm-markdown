module Markdown.OrderedList exposing (OrderedListMarker, parser)

import Helpers
import Parser
import Parser.Advanced as Advanced exposing (..)
import Parser.Extra exposing (chompOneOrMore, upTo)
import Parser.Token as Token
import Whitespace


type OrderedListMarker
    = Dot
    | Paren


type alias Parser a =
    Advanced.Parser String Parser.Problem a


type alias ListItem =
    { order : Int
    , intended : Int
    , marker : OrderedListMarker
    , body : String
    }


parser : Bool -> Parser ListItem
parser previousWasBody =
    let
        parseSubsequentItem start order marker mid ( end, body ) =
            if (end - mid) <= 4 then
                ListItem order (end - start) marker body

            else
                ListItem order (mid - start + 1) marker (String.repeat (end - mid - 1) " " ++ body)
    in
    succeed parseSubsequentItem
        |= getCol
        |= backtrackable
            (if previousWasBody then
                succeed identity
                    |. upTo 3 Whitespace.space
                    |= positiveIntegerMaxOf9Digits
                    |> andThen validateStartsWith1

             else
                succeed identity
                    |. upTo 3 Whitespace.space
                    |= positiveIntegerMaxOf9Digits
            )
        |= backtrackable orderedListMarkerParser
        |= getCol
        |= (if previousWasBody then
                oneOf
                    [ succeed Tuple.pair
                        |. chompOneOrMore Whitespace.isSpaceOrTab
                        |= getCol
                        |= Advanced.getChompedString Helpers.chompUntilLineEndOrEnd
                        |. Helpers.lineEndOrEnd
                    ]

            else
                oneOf
                    [ succeed ( 2, "" )
                        |. Helpers.lineEndOrEnd
                    , succeed Tuple.pair
                        |. chompOneOrMore Whitespace.isSpaceOrTab
                        |= getCol
                        |= Advanced.getChompedString Helpers.chompUntilLineEndOrEnd
                        |. Helpers.lineEndOrEnd
                    ]
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


{-| Lists inside a paragraph, or after a paragraph without a line break, must start with index 1.
-}
validateStartsWith1 : Int -> Parser Int
validateStartsWith1 parsed =
    case parsed of
        1 ->
            Advanced.succeed parsed

        _ ->
            Advanced.problem (Parser.Problem "Lists inside a paragraph or after a paragraph without a blank line must start with 1")


orderedListMarkerParser : Parser OrderedListMarker
orderedListMarkerParser =
    Advanced.oneOf
        [ succeed Dot
            |. Advanced.symbol Token.dot
        , succeed Paren
            |. Advanced.symbol Token.closingParen
        ]
