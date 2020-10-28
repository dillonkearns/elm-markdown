module ThematicBreak exposing (ThematicBreak(..), parser)

import Whitespace
import Helpers
import Parser
import Parser.Advanced as Advanced exposing (..)
import Parser.Token as Token


type alias Parser a =
    Advanced.Parser String Parser.Problem a


type ThematicBreak
    = ThematicBreak


parser : Parser ThematicBreak
parser =
    -- a thematic break can be preceded by up to 3 spaces.
    -- but in most documents it will be at the start of the line
    -- so we optimize for that case
    oneOf
        [ succeed identity
            |. Whitespace.space
            |. oneOf [ Whitespace.space, succeed () ]
            |. oneOf [ Whitespace.space, succeed () ]
            |= parseThematicBreak
        , parseThematicBreak
        ]


parseThematicBreak : Parser ThematicBreak
parseThematicBreak =
    oneOf
        [ withChar '-'
        , withChar '*'
        , withChar '_'
        ]


{-| Per the commonmark spec:

> a sequence of three or more matching -, \_, or \* characters,
> each followed optionally by any number of spaces, forms a horizontal rule.

-}
withChar : Char -> Parser ThematicBreak
withChar tchar =
    let
        token =
            Token.parseString (String.fromChar tchar)
    in
    succeed ThematicBreak
        |. token
        |. whitespace
        |. token
        |. whitespace
        |. token
        |. chompWhile (\c -> c == tchar || Whitespace.isSpaceOrTab c)
        |. Helpers.lineEndOrEnd


whitespace : Parser ()
whitespace =
    chompWhile Whitespace.isSpaceOrTab
