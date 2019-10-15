module Markdown.List exposing (..)

import Html exposing (Html)
import Html.Attributes as Attr
import Parser
import Parser.Advanced as Advanced exposing (..)


type alias Parser a =
    Advanced.Parser String Parser.Problem a


type alias ListItem =
    String


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
        markerOption : String -> Parser String
        markerOption marker =
            Advanced.getChompedString (Advanced.symbol (Advanced.Token marker (Parser.ExpectingSymbol marker)))
    in
    Advanced.oneOf
        [ markerOption "-"
        , markerOption "+"
        , markerOption "*"
        ]


openingItemParser : Parser ( String, ListItem )
openingItemParser =
    succeed Tuple.pair
        |= listMarkerParser
        |. chompWhile (\c -> c == ' ')
        |= Advanced.getChompedString (Advanced.chompUntilEndOr "\n")
        |. Advanced.symbol (Advanced.Token "\n" (Parser.ExpectingSymbol "\n"))


singleItemParser : String -> Parser ListItem
singleItemParser listMarker =
    succeed identity
        |. Advanced.symbol (Advanced.Token listMarker (Parser.ExpectingSymbol listMarker))
        |. chompWhile (\c -> c == ' ')
        |= Advanced.getChompedString (Advanced.chompUntilEndOr "\n")
        |. Advanced.symbol (Advanced.Token "\n" (Parser.ExpectingSymbol "\n"))


statementsHelp : String -> ListItem -> List ListItem -> Parser (Step (List ListItem) (List ListItem))
statementsHelp listMarker firstItem revStmts =
    oneOf
        [ succeed
            (\offsetBefore stmt offsetAfter ->
                -- let
                --     madeProgress =
                --         offsetAfter
                --             > offsetBefore
                --             |> Debug.log "progress"
                -- in
                -- if madeProgress then
                Loop (stmt :: revStmts)
             --
             -- else
             --     Done (List.reverse (stmt :: revStmts))
            )
            |= Advanced.getOffset
            |= singleItemParser listMarker
            |= Advanced.getOffset

        -- TODO this is causing files to require newlines
        -- at the end... how do I avoid this?
        -- |. symbol (Advanced.Token "\n" (Parser.Expecting "newline"))
        , succeed ()
            |> map (\_ -> Done (firstItem :: List.reverse revStmts))
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
