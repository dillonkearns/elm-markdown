module Markdown.ListItem exposing (Completion(..), ListItem(..), parser)

import Helpers
import Parser
import Parser.Advanced as Advanced exposing (..)
import Parser.Extra exposing (oneOrMore, zeroOrMore)


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
            |= taskItemParser
            |. zeroOrMore Helpers.isSpacebar
            |= Advanced.getChompedString (Advanced.chompUntilEndOr "\n")
            |. oneOf
                [ Advanced.symbol (Advanced.Token "\n" (Parser.ExpectingSymbol "\\n"))
                , Advanced.end (Parser.Expecting "End of input")
                ]
        , succeed PlainItem
            |= Advanced.getChompedString (Advanced.chompUntilEndOr "\n")
            |. oneOf
                [ Advanced.symbol (Advanced.Token "\n" (Parser.ExpectingSymbol "\\n"))
                , Advanced.end (Parser.Expecting "End of input")
                ]
        ]


taskItemParser : Parser Completion
taskItemParser =
    oneOf
        [ succeed Complete
            |. Advanced.symbol (Advanced.Token "[x] " (Parser.ExpectingSymbol "[x] "))
        , succeed Complete
            |. Advanced.symbol (Advanced.Token "[X] " (Parser.ExpectingSymbol "[X] "))
        , succeed Incomplete
            |. Advanced.symbol (Advanced.Token "[ ] " (Parser.ExpectingSymbol "[ ] "))
        ]
