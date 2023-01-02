module Markdown.Heading exposing (Parser, parser)

import Helpers
import Markdown.RawBlock exposing (RawBlock(..), UnparsedInlines(..))
import Parser
import Parser.Advanced as Advanced exposing ((|.), (|=), andThen, chompWhile, getChompedString, oneOf, spaces, succeed, symbol)
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
                            startSpace : Int
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
                            level : Int
                            level =
                                String.length additionalHashes + 1
                        in
                        if level >= 7 then
                            Advanced.problem (Parser.Expecting "heading with < 7 #'s")

                        else
                            succeed level
                    )
           )
        |= oneOf
            [ succeed (UnparsedInlines "")
                |. symbol Token.newline
            , succeed identity
                |. oneOf
                    [ symbol Token.space
                    , symbol Token.tab
                    ]
                |= (Helpers.chompUntilLineEndOrEnd
                        |> Advanced.mapChompedString
                            (\headingText _ ->
                                headingText
                                    |> String.trim
                                    |> dropClosingSequence
                                    |> UnparsedInlines
                            )
                   )
            ]


isHash : Char -> Bool
isHash c =
    case c of
        '#' ->
            True

        _ ->
            False


dropClosingSequence : String -> String
dropClosingSequence headingString =
    let
        droppedTrailingHashesString : String
        droppedTrailingHashesString =
            headingString
                |> dropTrailingHashes
    in
    if (droppedTrailingHashesString |> String.endsWith " ") || (droppedTrailingHashesString |> String.isEmpty) then
        droppedTrailingHashesString |> String.trimRight

    else
        headingString


dropTrailingHashes : String -> String
dropTrailingHashes headingString =
    if headingString |> String.endsWith "#" then
        dropTrailingHashes (String.dropRight 1 headingString)

    else
        headingString
