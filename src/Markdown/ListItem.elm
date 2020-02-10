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
            |. oneOf
                [ Advanced.symbol (Advanced.Token "\n" (Parser.ExpectingSymbol "\\n"))
                , Advanced.end (Parser.Expecting "End of input")
                ]
        , succeed
            PlainItem
            |= Advanced.getChompedString (Advanced.chompUntilEndOr "\n")
            |. oneOf
                [ Advanced.symbol (Advanced.Token "\n" (Parser.ExpectingSymbol "\\n"))
                , Advanced.end (Parser.Expecting "End of input")
                ]
        ]
