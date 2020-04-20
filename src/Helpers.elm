module Helpers exposing (..)

import Parser
import Parser.Advanced as Advanced exposing (..)
import Parser.Token as Token


type alias Parser a =
    Advanced.Parser String Parser.Problem a


optionalWhitespace : Parser ()
optionalWhitespace =
    chompWhile isGfmWhitespace


requiredWhitespace : Parser ()
requiredWhitespace =
    chompIf isGfmWhitespace (Parser.Expecting "gfm whitespace")
        |. chompWhile isGfmWhitespace


isSpacebar : Char -> Bool
isSpacebar c =
    case c of
        ' ' ->
            True

        _ ->
            False


isNewline : Char -> Bool
isNewline character =
    case character of
        '\n' ->
            True

        _ ->
            False


isSpaceOrTab : Char -> Bool
isSpaceOrTab c =
    case c of
        ' ' ->
            True

        '\t' ->
            True

        _ ->
            False


{-| Whitespace as defined in the GFM spec
-}
isGfmWhitespace : Char -> Bool
isGfmWhitespace char =
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

        '\u{000D}' ->
            True

        _ ->
            False


{-| From <https://spec.commonmark.org/0.29/#line-ending>:

> is a newline (U+000A), a carriage return (U+000D) not followed by a newline, or a carriage return and a following newline.

-}
lineEnding : Parser ()
lineEnding =
    oneOf
        [ token Token.newline
        , backtrackable (token Token.carriageReturn)
            |. oneOf [ token Token.newline, succeed () ]
        ]


toToken : String -> Advanced.Token Parser.Problem
toToken str =
    Advanced.Token str (Parser.Expecting str)


upToThreeSpaces : Parser ()
upToThreeSpaces =
    oneOf
        [ spaceOrTab
            |. oneOf [ spaceOrTab, succeed () ]
            |. oneOf [ spaceOrTab, succeed () ]
        , succeed ()
        ]


spaceOrTab : Parser ()
spaceOrTab =
    chompIf isSpaceOrTab (Parser.Expecting "space or tab")


endOfLineOrFile : Parser ()
endOfLineOrFile =
    oneOf
        [ Advanced.symbol Token.newline
        , Advanced.end (Parser.Expecting "end of input")
        ]

