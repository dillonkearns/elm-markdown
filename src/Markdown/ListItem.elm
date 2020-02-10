module Markdown.ListItem exposing (Completion(..), ListItem(..), parser)

import Helpers
import Parser
import Parser.Advanced as Advanced exposing (..)
import Parser.Extra exposing (oneOrMore)


type ListItem
    = TaskItem Completion String
    | PlainItem String


type Completion
    = Incomplete
    | Complete


type alias Parser a =
    Advanced.Parser String Parser.Problem a


parser : Parser ListItem
parser =
    oneOf
        [ succeed TaskItem
            |. Advanced.symbol (Advanced.Token "[" (Parser.ExpectingSymbol "["))
            |= succeed Incomplete
            |= succeed "Task item"
            |. Advanced.getChompedString (Advanced.chompUntilEndOr "\n")
        , succeed
            (PlainItem "Plain item")
        ]


listMarkerParser : Parser String
listMarkerParser =
    let
        markerOption : String -> Parser String
        markerOption marker =
            Advanced.getChompedString (Advanced.symbol (Advanced.Token marker (Parser.ExpectingSymbol marker)))
    in
    Advanced.oneOf
        [ markerOption "-"
        , markerOption "+"
        , markerOption "*"
        ]
