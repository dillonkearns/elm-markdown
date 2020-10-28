module Markdown.OrderedList exposing (parser)

import Whitespace
import Helpers
import Markdown.RawBlock exposing (RawBlock(..))
import Parser
import Parser.Advanced as Advanced exposing (..)
import Parser.Extra exposing (chompOneOrMore)
import Parser.Token as Token


type alias Parser a =
    Advanced.Parser String Parser.Problem a


type alias ListItem =
    String


parser : Bool -> Parser ( Int, List ListItem )
parser previousWasBody =
    succeed parseSubsequentItems
        -- NOTE this is only a list item when there is at least one space after the marker
        -- so the first parts must be backtrackable.
        |= backtrackable
            (if previousWasBody then
                positiveIntegerMaxOf9Digits |> andThen validateStartsWith1

             else
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
        |. chompOneOrMore Whitespace.isSpaceOrTab
        |= Advanced.getChompedString Helpers.chompUntilLineEndOrEnd
        |. Helpers.lineEndOrEnd
        |> andThen identity


parseSubsequentItems : Int -> Token Parser.Problem -> ListItem -> Parser ( Int, List ListItem )
parseSubsequentItems startingIndex listMarker firstItem =
    loop [] (statementsHelp (singleItemParser listMarker))
        |> map (\items -> ( startingIndex, firstItem :: items ))


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
            |. chompOneOrMore Whitespace.isSpaceOrTab
            |= Advanced.getChompedString Helpers.chompUntilLineEndOrEnd
            |. Helpers.lineEndOrEnd
        , succeed ""
            |. Helpers.lineEndOrEnd
        ]


statementsHelp : Parser ListItem -> List ListItem -> Parser (Step (List ListItem) (List ListItem))
statementsHelp itemParser revStmts =
    oneOf
        [ itemParser
            |> Advanced.map (\stmt -> Loop (stmt :: revStmts))
        , succeed (Done (List.reverse revStmts))
        ]
