module Markdown.Inlines exposing (State, isUninteresting, nextStepWhenFoundBold, nextStepWhenFoundItalic, nextStepWhenFoundNothing, parse, parseHelp, toString)

import Char
import Helpers
import Html exposing (Html)
import Html.Attributes as Attr
import Markdown.Block as Block exposing (Inline, InlineStyle)
import Markdown.Link as Link exposing (Link)
import Parser
import Parser.Advanced as Advanced exposing (..)


toString : List Inline -> String
toString list =
    List.map .string list
        |> String.join "-"


type alias Parser a =
    Advanced.Parser String Parser.Problem a


isUninteresting : Char -> Bool
isUninteresting char =
    case char of
        '*' ->
            False

        '`' ->
            False

        '[' ->
            False

        '!' ->
            False

        _ ->
            True


type alias State =
    ( InlineStyle, List Inline, Maybe String )


nextStepWhenFoundBold : State -> String -> Step State (List Inline)
nextStepWhenFoundBold ( currStyle, revStyledStrings, _ ) string =
    Loop
        ( { currStyle | isBold = not currStyle.isBold }
        , { style = currStyle, string = string } :: revStyledStrings
        , Nothing
        )


nextStepWhenFoundLink : Link -> State -> String -> Step State (List Inline)
nextStepWhenFoundLink link ( currStyle, revStyledStrings, _ ) string =
    case link of
        Link.Link record ->
            Loop
                ( currStyle
                , { style = { currStyle | link = Just { title = record.title, destination = Block.Link record.destination } }, string = record.description }
                    :: { style = currStyle, string = string }
                    :: revStyledStrings
                , Nothing
                )

        Link.Image record ->
            Loop
                ( currStyle
                , { style = { currStyle | link = Just { title = Nothing, destination = Block.Image record.src } }, string = record.alt }
                    :: { style = currStyle, string = string }
                    :: revStyledStrings
                , Nothing
                )


nextStepWhenFoundCode : State -> String -> Step State (List Inline)
nextStepWhenFoundCode ( currStyle, revStyledStrings, _ ) string =
    Loop
        ( { currStyle | isCode = not currStyle.isCode }
        , { style = currStyle, string = string } :: revStyledStrings
        , Nothing
        )


nextStepWhenFoundItalic : State -> String -> Step State (List Inline)
nextStepWhenFoundItalic ( currStyle, revStyledStrings, _ ) string =
    Loop
        ( { currStyle | isItalic = not currStyle.isItalic }
        , { style = currStyle, string = string } :: revStyledStrings
        , Nothing
        )


nextStepWhenFoundNothing : State -> String -> Step State (List Inline)
nextStepWhenFoundNothing ( currStyle, revStyledStrings, _ ) string =
    Done
        (List.reverse
            ({ style = currStyle, string = string } :: revStyledStrings)
            |> List.filter (\thing -> not <| Helpers.isEmptyString thing.string)
        )


nextStepWhenAllFailed : State -> String -> Step State (List Inline)
nextStepWhenAllFailed ( currStyle, revStyledStrings, _ ) string =
    Loop
        ( currStyle, revStyledStrings, Just string )


parse : Parser (List Inline)
parse =
    loop
        ( { isCode = False
          , isBold = False
          , isItalic = False
          , link = Nothing
          }
        , []
        , Nothing
        )
        parseHelp


parseHelp : State -> Parser (Step State (List Inline))
parseHelp (( _, _, allFailed ) as state) =
    andThen
        (\chompedString ->
            oneOf
                [ Link.parser
                    |> map (\link -> nextStepWhenFoundLink link state chompedString)
                , map
                    (\_ -> nextStepWhenFoundCode state chompedString)
                    (token (Token "`" (Parser.Expecting "`")))
                , map
                    (\_ -> nextStepWhenFoundBold state chompedString)
                    (token (Token "**" (Parser.Expecting "**")))
                , map
                    (\_ -> nextStepWhenFoundItalic state chompedString)
                    (token (Token "*" (Parser.Expecting "*")))
                , succeed identity
                    |= succeed (nextStepWhenFoundNothing state chompedString)
                    |. end (Parser.Expecting "End of inlines")
                , succeed (nextStepWhenAllFailed state chompedString)
                ]
        )
        (case allFailed of
            Nothing ->
                getChompedString (chompWhile isUninteresting)

            Just unhandledString ->
                succeed (\chomped -> unhandledString ++ chomped)
                    |= getChompedString (chompIf (\_ -> True) (Parser.Expecting "*"))
        )
