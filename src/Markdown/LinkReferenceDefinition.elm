module Markdown.LinkReferenceDefinition exposing (..)

import Helpers
import Markdown.Helpers
import Parser
import Parser.Advanced as Advanced exposing (..)
import Parser.Extra
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
        Helpers.upToThreeSpacesThen <|
            succeed
                (\label destination title ->
                    ( label
                    , { destination = destination, title = title }
                    )
                )
                |= labelParser
                -- whitespace that can contain up to 1 newline
                |. chompWhile Helpers.isSpaceOrTab
                |. oneOf
                    [ chompIf (\c -> c == '\n') (Parser.Expecting "newline")
                    , succeed ()
                    ]
                |. chompWhile Helpers.isSpaceOrTab
                -- rest
                |= destinationParser
                |= titleParser


labelParser : Parser String
labelParser =
    succeed Markdown.Helpers.prepareRefLabel
        |. chompIf (\c -> c == '[') (Parser.Expecting "[")
        |= getChompedString (chompWhile (\c -> c /= ']'))
        |. chompIf (\c -> c == ']') (Parser.Expecting "]")
        |. chompIf (\c -> c == ':') (Parser.Expecting ":")


destinationParser : Parser String
destinationParser =
    inContext "link destination" <|
        oneOf
            [ succeed Url.percentEncode
                |. chompIf (\c -> c == '<') (Parser.Expecting (String.fromChar '<'))
                |= getChompedString (chompWhile (\c -> c /= '>'))
                |. chompIf (\c -> c == '>') (Parser.Expecting (String.fromChar '>'))
            , Parser.Extra.oneOrMore (\c -> not <| Helpers.isGfmWhitespace c)
                |> getChompedString
            ]


titleParser : Parser (Maybe String)
titleParser =
    let
        inDoubleQuotes =
            succeed Just
                |. chompIf (\c -> c == '"') (Parser.Expecting "\"")
                |= (chompWhile (\c -> c /= '"')
                        |> getChompedString
                        |> andThen hasNoBlankLine
                   )
                |. chompIf (\c -> c == '"') (Parser.Expecting "\"")
                |. onlyWhitespaceTillNewline

        inSingleQuotes =
            succeed Just
                |. chompIf (\c -> c == '\'') (Parser.Expecting "'")
                |= (chompWhile (\c -> c /= '\'')
                        |> getChompedString
                        |> andThen hasNoBlankLine
                   )
                |. chompIf (\c -> c == '\'') (Parser.Expecting "'")
                |. onlyWhitespaceTillNewline
    in
    inContext "title" <|
        oneOf
            [ succeed identity
                |. Helpers.requiredWhitespace
                |= oneOf
                    [ inDoubleQuotes
                    , inSingleQuotes
                    , succeed Nothing
                    ]
            , succeed Nothing
            ]


hasNoBlankLine : String -> Parser String
hasNoBlankLine str =
    if String.contains "\n\n" str then
        Advanced.problem (Parser.Expecting "no blank line")

    else
        Advanced.succeed str


onlyWhitespaceTillNewline : Parser ()
onlyWhitespaceTillNewline =
    chompWhile (\c -> c /= '\n' && Helpers.isGfmWhitespace c)
        |. oneOf
            [ chompIf (\c -> c == '\n') (Parser.Expecting "\n")
            , Advanced.end (Parser.Expecting "end of file")
            ]
