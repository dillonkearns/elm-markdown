module Markdown.OrderedList exposing (parser)

import Helpers
import Markdown.RawBlock exposing (RawBlock(..))
import Parser
import Parser.Advanced as Advanced exposing (..)
import Parser.Extra exposing (oneOrMore)
import Parser.Token as Token


type alias Parser a =
    Advanced.Parser String Parser.Problem a


type alias ListItem =
    String


parser : Maybe RawBlock -> Parser ( Int, List ListItem )
parser lastBlock =
    openingItemParser lastBlock
        |> andThen
            (\( startingIndex, listMarker, firstItem ) ->
                loop [] (statementsHelp listMarker)
                    |> map (\items -> ( startingIndex, firstItem :: items ))
            )


positiveIntegerMaxOf9Digits : Parser Int
positiveIntegerMaxOf9Digits =
    Parser.Extra.positiveInteger
        |> Advanced.andThen
            (\parsed ->
                if parsed <= 999999999 then
                    Advanced.succeed parsed

                else
                    Advanced.problem (Parser.Problem "Starting numbers must be nine digits or less.")
            )


openingItemParser : Maybe RawBlock -> Parser ( Int, Token Parser.Problem, ListItem )
openingItemParser lastBlock =
    -- NOTE this is only a list item when there is at least one space after the marker
    -- so the first parts must be backtrackable.
    succeed (\startingIndex marker item -> ( startingIndex, marker, item ))
        |= backtrackable
            (case lastBlock of
                Just (Body _) ->
                    positiveIntegerMaxOf9Digits |> andThen validateStartsWith1

                _ ->
                    positiveIntegerMaxOf9Digits
            )
        |= backtrackable
            (Advanced.oneOf
                [ succeed Token.dot
                    |. Advanced.symbol Token.dot
                , succeed Token.closingParen
                    |. Advanced.symbol Token.closingParen
                ]
            )
        |. oneOrMore Helpers.isSpacebar
        |= Advanced.getChompedString (Advanced.chompUntilEndOr "\n")
        |. Advanced.symbol Token.newline


{-| Lists inside a paragraph, or after a paragraph without a line break, must start with index 1.
-}
validateStartsWith1 : Int -> Parser Int
validateStartsWith1 parsed =
    case parsed of
        1 ->
            Advanced.succeed parsed

        _ ->
            Advanced.problem (Parser.Problem "Lists inside a paragraph or after a paragraph without a blank line must start with 1")


singleItemParser : Token Parser.Problem -> Parser ListItem
singleItemParser listMarker =
    succeed identity
        |. backtrackable
            (Parser.Extra.positiveInteger
                |. Advanced.symbol listMarker
            )
        |= itemBody


itemBody : Parser ListItem
itemBody =
    oneOf
        [ succeed identity
            |. oneOrMore Helpers.isSpacebar
            |= Advanced.getChompedString (Advanced.chompUntilEndOr "\n")
            |. endOrNewline
        , succeed ""
            |. endOrNewline
        ]


endOrNewline : Parser ()
endOrNewline =
    oneOf
        [ Advanced.symbol Token.newline
        , Advanced.end (Parser.Expecting "end of input")
        ]


statementsHelp : Token Parser.Problem -> List ListItem -> Parser (Step (List ListItem) (List ListItem))
statementsHelp listMarker revStmts =
    oneOf
        [ succeed (\stmt -> Loop (stmt :: revStmts))
            |= singleItemParser listMarker
        , succeed (Done (List.reverse revStmts))
        ]
