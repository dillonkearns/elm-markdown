module Helpers exposing (..)

import Parser
import Parser.Advanced as Advanced exposing (..)


type alias Parser a =
    Advanced.Parser String Parser.Problem a


optionalWhitespace : Parser ()
optionalWhitespace =
    succeed ()
        |. chompWhile isGfmWhitespace


requiredWhitespace : Parser ()
requiredWhitespace =
    succeed ()
        |. chompIf isGfmWhitespace (Parser.Expecting "whitespace")
        |. chompWhile isGfmWhitespace


isEmptyString : String -> Bool
isEmptyString string =
    case string of
        "" ->
            True

        _ ->
            False


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
        [ token (toToken "\n")
        , token (toToken (carriageReturn ++ "\n"))
        , token (toToken carriageReturn)
        ]


carriageReturn : String
carriageReturn =
    "\u{000D}"


toToken : String -> Advanced.Token Parser.Problem
toToken str =
    Advanced.Token str (Parser.Expecting str)
