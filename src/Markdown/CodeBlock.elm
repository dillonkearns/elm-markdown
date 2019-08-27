module Markdown.CodeBlock exposing (..)

import Parser
import Parser.Advanced as Advanced exposing (..)


type alias Parser a =
    Advanced.Parser String Parser.Problem a


parser : Parser CodeBlock
parser =
    succeed
        (\body ->
            { body = body
            , language = Nothing
            }
        )
        |. Advanced.symbol (Advanced.Token "```\n" (Parser.ExpectingSymbol "```"))
        |= getChompedString (Advanced.chompUntil (Advanced.Token "\n```" (Parser.Problem "Expecting ending code fence.")))



-- |. Advanced.symbol (Advanced.Token "```" (Parser.ExpectingSymbol "```"))


type alias CodeBlock =
    { body : String
    , language : Maybe String
    }
