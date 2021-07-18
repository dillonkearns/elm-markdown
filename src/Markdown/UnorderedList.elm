module Markdown.UnorderedList exposing (UnorderedListMarker, parser)

import Helpers
import Markdown.ListItem as ListItem exposing (ListItem(..))
import Parser exposing (Problem)
import Parser.Advanced as Advanced exposing (..)
import Parser.Extra as Extra exposing (chompOneOrMore)
import Parser.Token as Token
import Whitespace


type UnorderedListMarker
    = Minus
    | Plus
    | Asterisk


type alias Parser a =
    Advanced.Parser String Parser.Problem a


parser : Bool -> Parser ( UnorderedListMarker, Int, ListItem )
parser previousWasBody =
    let
        parseSubsequentItems start listmaker mid ( end, firstItem ) =
            if (end - mid) <= 4 then
                ( listmaker, end - start, firstItem )

            else
                let
                    intendedCodeItem =
                        case firstItem of
                            TaskItem completion string ->
                                TaskItem completion (String.repeat (end - mid - 1) " " ++ string)

                            PlainItem string ->
                                PlainItem (String.repeat (end - mid - 1) " " ++ string)

                            EmptyItem ->
                                EmptyItem
                in
                ( listmaker, mid - start + 1, intendedCodeItem )
    in
    succeed parseSubsequentItems
        |= getCol
        |= backtrackable unorderedListMarkerParser
        |= getCol
        |= (if previousWasBody then
                oneOf
                    [ succeed Tuple.pair
                        |. chompOneOrMore Whitespace.isSpaceOrTab
                        |= getCol
                        |= ListItem.parser
                    ]

            else
                oneOf
                    [ succeed ( 2, EmptyItem )
                        |. Helpers.lineEndOrEnd
                    , succeed Tuple.pair
                        |. chompOneOrMore Whitespace.isSpaceOrTab
                        |= getCol
                        |= ListItem.parser
                    ]
           )


unorderedListMarkerParser : Parser UnorderedListMarker
unorderedListMarkerParser =
    oneOf
        [ succeed Minus
            |. Extra.upTo 3 Whitespace.space
            |. Advanced.symbol (Advanced.Token "-" (Parser.ExpectingSymbol "-"))
        , succeed Plus
            |. Advanced.symbol (Advanced.Token "+" (Parser.ExpectingSymbol "+"))
        , succeed Asterisk
            |. Advanced.symbol (Advanced.Token "*" (Parser.ExpectingSymbol "*"))
        ]
