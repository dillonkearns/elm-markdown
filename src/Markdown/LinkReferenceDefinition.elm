module Markdown.LinkReferenceDefinition exposing (..)

import Helpers
import Parser
import Parser.Advanced as Advanced exposing (..)


type alias Parser a =
    Advanced.Parser String Parser.Problem a


parser : Parser LinkReferenceDefinition
parser =
    succeed
        (\label destination title ->
            ( label
            , { destination = destination, title = title }
            )
        )
        -- TODO indented up to 3 spaces
        |. token (toToken "[")
        |= getChompedString (chompUntilEndOr "]")
        |. token (toToken "]:")
        -- TODO up to 1 line ending
        |. Helpers.optionalWhitespace
        |= getChompedString (chompWhile (\c -> not <| Helpers.isGfmWhitespace c))
        |. Helpers.requiredWhitespace
        |= titleParser


titleParser : Parser (Maybe String)
titleParser =
    oneOf
        [ succeed Just
            |. token (toToken "\"")
            |= getChompedString (chompUntilEndOr "\"")
            |. token (toToken "\"")
        ]


toToken : String -> Advanced.Token Parser.Problem
toToken str =
    Advanced.Token str (Parser.Expecting str)


type alias LinkReferenceDefinition =
    ( String
    , { destination : String
      , title : Maybe String
      }
    )
