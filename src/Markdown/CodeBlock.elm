module Markdown.CodeBlock exposing (..)

import Parser
import Parser.Advanced as Advanced exposing (..)


type alias Parser a =
    Advanced.Parser String Parser.Problem a


parser : Parser CodeBlock
parser =
    succeed
        (\language body ->
            { body = body
            , language =
                if language == "" then
                    Nothing

                else
                    Just language
            }
        )
        |. Advanced.symbol (Advanced.Token "```" (Parser.ExpectingSymbol "```"))
        |= getChompedString (chompUntil (Advanced.Token "\n" (Parser.Problem "Expecting ending code fence.")))
        |. Advanced.symbol (Advanced.Token "\n" (Parser.ExpectingSymbol "\n"))
        |= getChompedString (Advanced.chompUntil (Advanced.Token "\n```" (Parser.Problem "Expecting ending code fence.")))



-- |. Advanced.symbol (Advanced.Token "```" (Parser.ExpectingSymbol "```"))


type alias CodeBlock =
    { body : String
    , language : Maybe String
    }
