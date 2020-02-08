module Helpers exposing (..)


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
