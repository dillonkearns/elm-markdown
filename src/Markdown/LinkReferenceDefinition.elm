module Markdown.LinkReferenceDefinition exposing (..)

import Whitespace
import Helpers
import Markdown.Helpers
import Parser
import Parser.Advanced as Advanced exposing (..)
import Parser.Extra
import Parser.Token as Token
import Url


type alias Parser a =
    Advanced.Parser String Parser.Problem a


type alias LinkReferenceDefinition =
    ( String
    , { destination : String
      , title : Maybe String
      }
    )


parser : Parser LinkReferenceDefinition
parser =
    inContext "link reference definition" <|
        succeed
            (\label destination title ->
                ( label
                , { destination = destination, title = title }
                )
            )
            |. Whitespace.upToThreeSpaces
            |= labelParser
            -- whitespace that can contain up to 1 newline
            |. chompWhile Whitespace.isSpaceOrTab
            |. oneOf
                [ Whitespace.lineEnd
                , succeed ()
                ]
            |. chompWhile Whitespace.isSpaceOrTab
            -- rest
            |= destinationParser
            |= titleParser


labelParser : Parser String
labelParser =
    succeed Markdown.Helpers.prepareRefLabel
        |. symbol Token.openingSquareBracket
        |= getChompedString (chompUntil Token.closingSquareBracket)
        |. symbol (Token "]:" (Parser.Expecting "]:"))


destinationParser : Parser String
destinationParser =
    inContext "link destination" <|
        oneOf
            [ succeed Url.percentEncode
                |. symbol Token.lessThan
                |= getChompedString (chompUntil Token.greaterThan)
                |. symbol Token.greaterThan
            , Parser.Extra.chompOneOrMore (not << Whitespace.isWhitespace)
                |> getChompedString
            ]


titleParser : Parser (Maybe String)
titleParser =
    let
        inDoubleQuotes =
            succeed Just
                |. symbol Token.doubleQuote
                |= (chompUntil Token.doubleQuote
                        |> getChompedString
                        |> andThen hasNoBlankLine
                   )
                |. symbol Token.doubleQuote
                |. onlyWhitespaceTillNewline

        inSingleQuotes =
            succeed Just
                |. symbol Token.singleQuote
                |= (chompUntil Token.singleQuote
                        |> getChompedString
                        |> andThen hasNoBlankLine
                   )
                |. symbol Token.singleQuote
                |. onlyWhitespaceTillNewline
    in
    inContext "title" <|
        oneOf
            [ succeed identity
                |. Whitespace.requiredWhitespace
                |= oneOf
                    [ inDoubleQuotes
                    , inSingleQuotes
                    , succeed Nothing
                    ]
                |> backtrackable
            , succeed Nothing
                |. onlyWhitespaceTillNewline
            ]


hasNoBlankLine : String -> Parser String
hasNoBlankLine str =
    if String.contains "\n\n" str then
        Advanced.problem (Parser.Expecting "no blank line")

    else
        Advanced.succeed str


onlyWhitespaceTillNewline : Parser ()
onlyWhitespaceTillNewline =
    chompWhile (\c -> not (Whitespace.isLineEnd c) && Whitespace.isWhitespace c)
        |. Helpers.lineEndOrEnd
