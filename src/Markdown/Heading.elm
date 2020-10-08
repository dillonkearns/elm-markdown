module Markdown.Heading exposing (..)

import Markdown.RawBlock exposing (Attribute, RawBlock(..), UnparsedInlines(..))
import Parser
import Parser.Advanced as Advanced exposing ((|.), (|=), Nestable(..), Step(..), andThen, chompIf, chompWhile, getChompedString, spaces, succeed, symbol)
import Parser.Token as Token


type alias Parser a =
    Advanced.Parser String Parser.Problem a


parser : Parser RawBlock
parser =
    succeed Heading
        |. (getChompedString spaces
                |> andThen
                    (\startingSpaces ->
                        let
                            startSpace =
                                String.length startingSpaces
                        in
                        if startSpace >= 4 then
                            Advanced.problem (Parser.Expecting "heading with < 4 spaces in front")

                        else
                            succeed startSpace
                    )
           )
        |. symbol Token.hash
        |= (getChompedString (chompWhile isHash)
                |> andThen
                    (\additionalHashes ->
                        let
                            level =
                                String.length additionalHashes + 1
                        in
                        if level >= 7 then
                            Advanced.problem (Parser.Expecting "heading with < 7 #'s")

                        else
                            succeed level
                    )
           )
        |= (Advanced.chompUntilEndOr "\n"
                |> Advanced.mapChompedString
                    (\headingText _ ->
                        headingText
                            |> String.trimLeft
                            |> dropTrailingHashes
                            |> UnparsedInlines
                    )
           )


isHash : Char -> Bool
isHash c =
    case c of
        '#' ->
            True

        _ ->
            False


dropTrailingHashes : String -> String
dropTrailingHashes headingString =
    if headingString |> String.endsWith "#" then
        String.dropRight 1 headingString
            |> String.trimRight
            |> dropTrailingHashes

    else
        headingString
