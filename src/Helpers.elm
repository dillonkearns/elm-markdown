module Helpers exposing (Parser, chompUntilLineEndOrEnd, endOfFile, lineEndOrEnd)

import Parser
import Parser.Advanced as Advanced exposing (..)
import Whitespace


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
