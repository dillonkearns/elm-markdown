module Markdown.UnorderedList exposing (UnorderedListMarker, parser)

import Helpers
import Markdown.ListItem as ListItem exposing (ListItem(..))
import Parser exposing (Problem)
import Parser.Advanced as Advanced exposing (..)
import Parser.Extra exposing (chompOneOrMore)
import Parser.Token as Token
import Whitespace


type UnorderedListMarker
    = Minus
    | Plus
    | Asterisk


type alias Parser a =
    Advanced.Parser String Parser.Problem a


parser : Bool -> Parser ( UnorderedListMarker, ListItem )
parser previousWasBody =
    let
        parseSubsequentItems listmaker firstItem =
            ( listmaker, firstItem )
    in
    succeed parseSubsequentItems
        |= backtrackable unorderedListMarkerParser
        |= listItemParser previousWasBody


unorderedListMarkerParser : Parser UnorderedListMarker
unorderedListMarkerParser =
    oneOf
        [ succeed Minus
            |. Advanced.symbol (Advanced.Token "-" (Parser.ExpectingSymbol "-"))
        , succeed Plus
            |. Advanced.symbol (Advanced.Token "+" (Parser.ExpectingSymbol "+"))
        , succeed Asterisk
            |. Advanced.symbol (Advanced.Token "*" (Parser.ExpectingSymbol "*"))
        ]


listItemParser : Bool -> Parser ListItem
listItemParser previousWasBody =
    if previousWasBody then
        oneOf
            [ succeed identity
                |. chompOneOrMore Whitespace.isSpaceOrTab
                |= ListItem.parser
            ]

    else
        oneOf
            [ succeed EmptyItem
                |. Helpers.lineEndOrEnd
            , succeed identity
                |. chompOneOrMore Whitespace.isSpaceOrTab
                |= ListItem.parser
            ]
