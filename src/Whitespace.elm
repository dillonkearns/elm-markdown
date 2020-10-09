module Whitespace exposing (..)

import Parser
import Parser.Advanced as Advanced exposing (..)
import Parser.Token as Token


type alias Parser a =
    Advanced.Parser String Parser.Problem a


-- Assertions


{-| Whitespace as defined in the GFM spec
-}
isWhitespace : Char -> Bool
isWhitespace char =
    case char of
        ' ' ->
            True

        '\n' ->
            True

        '\t' ->
            True

        '\u{000B}' ->
            True

        '\u{000C}' ->
            True

        '\r' ->
            True

        _ ->
            False


{-| Line ending as defined in the GFM spec
    Note that a line ending can also be a carriage return
    followed by a newline.
-}
isLineEnd : Char -> Bool
isLineEnd char =
    case char of
        '\n' -> -- Newline
            True

        '\r' -> -- Carriage return
            True

        _ ->
            False


isSpaceOrTab : Char -> Bool
isSpaceOrTab char =
    case char of
        ' ' ->
            True

        '\t' ->
            True

        _ ->
            False


isSpace : Char -> Bool
isSpace =
    (==) ' '


-- Parsers


space : Parser ()
space =
    token Token.space


tab : Parser ()
tab =
    token Token.tab


-- A note about the relationship between tabs and spaces:
-- In places where tabs can define block structure, they are to be
-- treated as four spaces i.e. a single tab can create an indented
-- code block.
upToThreeSpaces : Parser ()
upToThreeSpaces =
    oneOf
        [ space
            |. oneOf [ space, succeed () ]
            |. oneOf [ space, succeed () ]
        , succeed ()
        ]


{-| From <https://spec.commonmark.org/0.29/#line-ending>:

> is a newline (U+000A), a carriage return (U+000D) not followed by a newline, or a carriage return and a following newline.

-}
lineEnd : Parser ()
lineEnd =
    oneOf
        [ token Token.newline
        , token Token.carriageReturn
            |. oneOf [ token Token.newline, succeed () ]
        ]


-- Chompers


optionalWhitespace : Parser ()
optionalWhitespace =
    chompWhile isWhitespace


requiredWhitespace : Parser ()
requiredWhitespace =
    chompIf isWhitespace (Parser.Expecting "Required whitespace")
        |. chompWhile isWhitespace

