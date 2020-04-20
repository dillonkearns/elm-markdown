module Markdown.UnorderedList exposing (parser)

import Helpers
import Markdown.ListItem as ListItem exposing (ListItem)
import Parser
import Parser.Advanced as Advanced exposing (..)
import Parser.Extra exposing (oneOrMore)


type alias Parser a =
    Advanced.Parser String Parser.Problem a


parser : Parser (List ListItem)
parser =
    openingItemParser
        |> andThen
            (\( listMarker, firstItem ) ->
                loop [] (statementsHelp listMarker firstItem)
            )


listMarkerParser : Parser String
listMarkerParser =
    let
        markerOption : String -> Parser ()
        markerOption marker =
            Advanced.symbol (Advanced.Token marker (Parser.ExpectingSymbol marker))
    in
    Advanced.oneOf
        [ markerOption "-"
        , markerOption "+"
        , markerOption "*"
        ]
        |> Advanced.getChompedString


openingItemParser : Parser ( String, ListItem )
openingItemParser =
    succeed Tuple.pair
        |= (backtrackable listMarkerParser
                |. oneOrMore Helpers.isSpaceOrTab
           )
        |= ListItem.parser



--|. Advanced.symbol (Advanced.Token "\n" (Parser.ExpectingSymbol "\n"))


singleItemParser : String -> Parser ListItem
singleItemParser listMarker =
    succeed identity
        |. backtrackable
            (Advanced.symbol (Advanced.Token listMarker (Parser.ExpectingSymbol listMarker)))
        |= itemBody


itemBody : Parser ListItem
itemBody =
    oneOf
        [ succeed identity
            |. backtrackable (oneOrMore Helpers.isSpaceOrTab)
            |. commit ""
            |= ListItem.parser
        , succeed (ListItem.PlainItem "")
            |. Advanced.symbol (Advanced.Token "\n" (Parser.ExpectingSymbol "\\n"))
        ]


statementsHelp : String -> ListItem -> List ListItem -> Parser (Step (List ListItem) (List ListItem))
statementsHelp listMarker firstItem revStmts =
    oneOf
        [ succeed
            (\stmt ->
                Loop (stmt :: revStmts)
            )
            |= singleItemParser listMarker

        -- TODO this is causing files to require newlines
        -- at the end... how do I avoid this?
        -- |. symbol (Advanced.Token "\n" (Parser.Expecting "newline"))
        , succeed (Done (firstItem :: List.reverse revStmts))
        ]



-- |= getChompedString
--     (chompUntilEndOr
--         (Advanced.Token "\n" (Parser.ExpectingSymbol "\n"))
--     )
-- |. Advanced.symbol (Advanced.Token "]" (Parser.ExpectingSymbol "]"))
-- |. Advanced.symbol (Advanced.Token "(" (Parser.ExpectingSymbol "("))
-- |= getChompedString
--     (chompUntil (Advanced.Token ")" (Parser.ExpectingSymbol ")")))
-- |. Advanced.symbol (Advanced.Token ")" (Parser.ExpectingSymbol ")"))
-- isUninteresting : Char -> Bool
-- isUninteresting char =
--     char /= '*' && char /= '`'
