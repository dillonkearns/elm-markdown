module Helpers exposing (..)

import Whitespace
import Parser
import Parser.Advanced as Advanced exposing (..)
import Parser.Token as Token


type alias Parser a =
    Advanced.Parser String Parser.Problem a


toToken : String -> Advanced.Token Parser.Problem
toToken str =
    Advanced.Token str (Parser.Expecting str)


upToThree : Parser () -> Parser ()
upToThree parser =
    oneOf
        [ parser
            |. oneOf [ parser, succeed () ]
            |. oneOf [ parser, succeed () ]
        , succeed ()
        ]


endOfFile : Parser ()
endOfFile =
    Advanced.end (Parser.Expecting "end of input")


lineEndOrEnd : Parser ()
lineEndOrEnd =
    oneOf
        [ Whitespace.lineEnd
        , Advanced.end (Parser.Expecting "End of input")
        ]


chompUntilLineEndOrEnd : Parser ()
chompUntilLineEndOrEnd =
    chompWhile (not << Whitespace.isLineEnd)
