module Markdown.List exposing (..)

import Browser
import Char
import Html exposing (Html)
import Html.Attributes as Attr
import Markdown.Inlines
import Parser
import Parser.Advanced as Advanced exposing (..)


type alias Parser a =
    Advanced.Parser String Parser.Problem a


parser : Parser (List (List Markdown.Inlines.StyledString))
parser =
    succeed
        (\string ->
            -- TODO use the Inline parser
            [ [ { string = string, style = { isCode = False, isBold = False, isItalic = False, link = Nothing } } ] ]
        )
        |. Advanced.symbol (Advanced.Token "-" (Parser.ExpectingSymbol "-"))
        |. chompWhile (\c -> c == ' ')
        |= Advanced.getChompedString (Advanced.chompUntilEndOr "\n")
