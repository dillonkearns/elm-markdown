module Markdown.UnorderedList exposing (UnorderedListMarker(..), parser)

import Helpers
import Markdown.ListItem as ListItem exposing (ListItem(..))
import Parser exposing (Problem)
import Parser.Advanced as Advanced exposing (..)
import Parser.Extra as Extra exposing (chompOneOrMore)
import Whitespace


type UnorderedListMarker
    = Minus
    | Plus
    | Asterisk


type alias Parser a =
    Advanced.Parser String Parser.Problem a


parser : Bool -> Parser ( UnorderedListMarker, Int, ListItem )
parser previousWasBody =
    succeed getIntendedCodeItem
        |= getCol
        |= backtrackable unorderedListMarkerParser
        |= getCol
        |= (if previousWasBody then
                unorderedListItemBodyParser

            else
                oneOf
                    [ unorderedListEmptyItemParser
                    , unorderedListItemBodyParser
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


unorderedListItemBodyParser : Parser ( Int, ListItem )
unorderedListItemBodyParser =
    succeed (\bodyStartPos item -> ( bodyStartPos, item ))
        |. chompOneOrMore Whitespace.isSpaceOrTab
        |= getCol
        |= ListItem.parser


unorderedListEmptyItemParser : Parser ( Int, ListItem )
unorderedListEmptyItemParser =
    succeed (\bodyStartPos -> ( bodyStartPos, EmptyItem ))
        |= getCol
        |. Helpers.lineEndOrEnd


getIntendedCodeItem markerStartPos listMarker markerEndPos ( bodyStartPos, item ) =
    let
        spaceNum =
            bodyStartPos - markerEndPos
    in
    if spaceNum <= 4 then
        ( listMarker, bodyStartPos - markerStartPos, item )

    else
        let
            intendedCodeItem =
                case item of
                    TaskItem completion string ->
                        TaskItem completion (String.repeat (spaceNum - 1) " " ++ string)

                    PlainItem string ->
                        PlainItem (String.repeat (spaceNum - 1) " " ++ string)

                    EmptyItem ->
                        EmptyItem
        in
        ( listMarker, markerEndPos - markerStartPos + 1, intendedCodeItem )
