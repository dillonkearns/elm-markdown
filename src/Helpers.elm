module Helpers exposing (..)

import Whitespace
import Parser
import Parser.Advanced as Advanced exposing (..)
import Parser.Token as Token


type alias Parser a =
    Advanced.Parser String Parser.Problem a


endOfFile : Parser ()
endOfFile =
    Advanced.end (Parser.Expecting "the end of the input")


lineEndOrEnd : Parser ()
lineEndOrEnd =
    oneOf
        [ Whitespace.lineEnd
        , endOfFile
        ]


chompUntilLineEndOrEnd : Parser ()
chompUntilLineEndOrEnd =
    chompWhile (not << Whitespace.isLineEnd)
