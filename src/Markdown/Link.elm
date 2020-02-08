module Markdown.Link exposing (..)

import Html exposing (Html)
import Html.Attributes as Attr
import Parser
import Parser.Advanced as Advanced exposing (..)


type alias Parser a =
    Advanced.Parser String Parser.Problem a


type Link
    = Link { description : String, title : Maybe String, destination : String }
    | Image { alt : String, src : String }


parser : Parser Link
parser =
    oneOf
        [ succeed
            (\alt src ->
                Image
                    { alt = alt
                    , src = src
                    }
            )
            |. Advanced.symbol (Advanced.Token "![" (Parser.ExpectingSymbol "["))
            |= getChompedString
                (chompUntil (Advanced.Token "]" (Parser.ExpectingSymbol "]")))
            |. Advanced.symbol (Advanced.Token "]" (Parser.ExpectingSymbol "]"))
            |. Advanced.symbol (Advanced.Token "(" (Parser.ExpectingSymbol "("))
            |= getChompedString
                (chompUntil (Advanced.Token ")" (Parser.ExpectingSymbol ")")))
            |. Advanced.symbol (Advanced.Token ")" (Parser.ExpectingSymbol ")"))
        , succeed
            (\description destination ->
                Link
                    { description = description
                    , title = Nothing
                    , destination = destination
                    }
            )
            |. Advanced.symbol (Advanced.Token "[" (Parser.ExpectingSymbol "["))
            |= getChompedString
                (chompUntil (Advanced.Token "]" (Parser.ExpectingSymbol "]")))
            |. Advanced.symbol (Advanced.Token "]" (Parser.ExpectingSymbol "]"))
            |. Advanced.symbol (Advanced.Token "(" (Parser.ExpectingSymbol "("))
            |= linkDestination
            |. Advanced.symbol (Advanced.Token ")" (Parser.ExpectingSymbol ")"))
        ]


linkDestination : Parser String
linkDestination =
    oneOf
        [ succeed identity
            |. Advanced.symbol (Advanced.Token "<" (Parser.ExpectingSymbol "<"))
            |= getChompedString
                (chompUntil (Advanced.Token ">" (Parser.ExpectingSymbol ">")))
            |. Advanced.symbol (Advanced.Token ">" (Parser.ExpectingSymbol ">"))
            |> andThen cantContainNewline
        , succeed identity
            |= getChompedString
                (chompUntil (Advanced.Token ")" (Parser.ExpectingSymbol ")")))
            |> andThen cantContainWhitespace
        ]


cantContainNewline : String -> Parser String
cantContainNewline destination =
    if String.contains "\n" destination then
        problem (Parser.Problem "Link destinations can't contain new lines")

    else
        succeed destination


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

        '\u{000D}' ->
            True

        _ ->
            False


cantContainWhitespace : String -> Parser String
cantContainWhitespace untrimmed =
    let
        destination =
            String.trim untrimmed
    in
    if String.any isWhitespace destination then
        problem (Parser.Problem "Link destinations can't contain whitespace, if you would like to include them please wrap your URL with < .. >")

    else
        succeed destination
