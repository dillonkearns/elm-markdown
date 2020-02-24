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
