module Markdown.ListItem exposing (Completion(..), ListItem(..), parser)

import Whitespace
import Helpers
import Parser
import Parser.Advanced as Advanced exposing (..)


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
            |. chompWhile Whitespace.isSpaceOrTab
        , succeed PlainItem
        ]
        |= Advanced.getChompedString Helpers.chompUntilLineEndOrEnd
        |. Helpers.lineEndOrEnd


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
