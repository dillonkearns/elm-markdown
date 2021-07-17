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
        parseSubsequentItems start listmaker ( end, firstItem ) =
            ( listmaker, end - start, firstItem )
    in
    succeed parseSubsequentItems
        |= getCol
        |= backtrackable unorderedListMarkerParser
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
