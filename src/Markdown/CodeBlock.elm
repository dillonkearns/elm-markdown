module Markdown.CodeBlock exposing (..)

import Helpers
import Parser
import Parser.Advanced as Advanced exposing (..)


type alias Parser a =
    Advanced.Parser String Parser.Problem a


parser : Parser CodeBlock
parser =
    parserHelp


openingCodeFence =
    oneOf
        [ Advanced.symbol (Advanced.Token "```" (Parser.ExpectingSymbol "```"))
            |. Advanced.chompWhile (\c -> c == '`')
            |> getChompedString
            |> map (\delimiter -> ( '`', delimiter ))
        , Advanced.symbol (Advanced.Token "~~~" (Parser.ExpectingSymbol "~~~"))
            |. Advanced.chompWhile (\c -> c == '~')
            |> getChompedString
            |> map (\delimiter -> ( '~', delimiter ))
        ]


parserHelp : Parser CodeBlock
parserHelp =
    openingCodeFence
        |> Advanced.andThen
            (\( char, delimiter ) ->
                succeed
                    (\language body ->
                        { body = body
                        , language =
                            if String.isEmpty language then
                                Nothing

                            else
                                Just language
                        }
                    )
                    |= getChompedString (chompUntil (Advanced.Token "\n" (Parser.Problem "Expecting newline")))
                    |. Advanced.symbol (Advanced.Token "\n" (Parser.ExpectingSymbol "\n"))
                    |= getChompedString (Advanced.chompUntilEndOr ("\n" ++ delimiter))
                    |. Advanced.symbol (Advanced.Token ("\n" ++ delimiter) (Parser.ExpectingSymbol delimiter))
                    |. chompWhile (\c -> c == char)
            )


type alias CodeBlock =
    { body : String
    , language : Maybe String
    }
