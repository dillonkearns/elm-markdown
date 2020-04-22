module ThematicBreak exposing (ThematicBreak(..), parser)

import Parser
import Parser.Advanced as Advanced exposing (..)
import Parser.Extra as Extra exposing (tokenHelp)


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
            |. singleSpace
            |. oneOf [ singleSpace, succeed () ]
            |. oneOf [ singleSpace, succeed () ]
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
            tokenHelp (String.fromChar tchar)
    in
    succeed ThematicBreak
        |. token
        |. whitespace
        |. token
        |. whitespace
        |. token
        |. chompWhile (\c -> c == tchar || isSpace c)
        |. oneOf
            [ tokenHelp "\n"
            , end (Parser.Expecting "end")
            ]


isSpace : Char -> Bool
isSpace c =
    case c of
        ' ' ->
            True

        '\t' ->
            True

        _ ->
            False


whitespace : Parser ()
whitespace =
    chompWhile isSpace


singleSpace : Parser ()
singleSpace =
    chompIf isSpace (Parser.Expecting "Space")
