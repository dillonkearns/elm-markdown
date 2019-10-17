module Markdown.OrderedList exposing (parser)

import Parser
import Parser.Advanced as Advanced exposing (..)
import Parser.Extra exposing (oneOrMore)


type alias Parser a =
    Advanced.Parser String Parser.Problem a


type alias ListItem =
    String


parser : Parser ( Int, List ListItem )
parser =
    openingItemParser
        |> andThen
            (\( startingIndex, listMarker, firstItem ) ->
                loop [] (statementsHelp listMarker firstItem)
                    |> map (\items -> ( startingIndex, items ))
            )


listMarkerParser : Parser ( Int, String )
listMarkerParser =
    let
        markerOption : String -> Parser String
        markerOption marker =
            Advanced.getChompedString (Advanced.symbol (Advanced.Token marker (Parser.ExpectingSymbol marker)))
    in
    succeed Tuple.pair
        |= Parser.Extra.positiveInteger
        |= Advanced.oneOf
            [ markerOption "."
            , markerOption ")"
            ]


openingItemParser : Parser ( Int, String, ListItem )
openingItemParser =
    succeed (\( startingIndex, marker ) item -> ( startingIndex, marker, item ))
        |= (backtrackable listMarkerParser
                |. oneOrMore (\c -> c == ' ')
           )
        |= Advanced.getChompedString (Advanced.chompUntilEndOr "\n")
        |. Advanced.symbol (Advanced.Token "\n" (Parser.ExpectingSymbol "\n"))


singleItemParser : String -> Parser ListItem
singleItemParser listMarker =
    succeed identity
        |. backtrackable
            (Parser.Extra.positiveInteger
                |. Advanced.symbol (Advanced.Token listMarker (Parser.ExpectingSymbol listMarker))
                |. oneOrMore (\c -> c == ' ')
            )
        |= Advanced.getChompedString (Advanced.chompUntilEndOr "\n")
        |. Advanced.symbol (Advanced.Token "\n" (Parser.ExpectingSymbol "\n"))


statementsHelp : String -> ListItem -> List ListItem -> Parser (Step (List ListItem) (List ListItem))
statementsHelp listMarker firstItem revStmts =
    oneOf
        [ succeed
            (\offsetBefore stmt offsetAfter ->
                Loop (stmt :: revStmts)
            )
            |= Advanced.getOffset
            |= singleItemParser listMarker
            |= Advanced.getOffset
        , succeed ()
            |> map (\_ -> Done (firstItem :: List.reverse revStmts))
        ]
