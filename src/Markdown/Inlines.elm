module Markdown.Inlines exposing (State, isUninteresting, nextStepWhenFoundBold, nextStepWhenFoundItalic, nextStepWhenFoundNothing, parse, parseHelp, toString)

import Char
import Helpers
import Markdown.Block as Block
import Markdown.Link as Link exposing (Link)
import Parser
import Parser.Advanced as Advanced exposing (..)


type alias Inline =
    { style : InlineStyle, string : String }


type alias InlineStyle =
    { isCode : Bool
    , isBold : Bool
    , isItalic : Bool
    , link : Maybe { title : Maybe String, destination : InlineLink }
    }


type InlineLink
    = Image String
    | Link String


toString : List Block.TopLevelInline -> String
toString list =
    "TODO"



--List.map .string list
--    |> String.join "-"


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
                , { style = { currStyle | link = Just { title = record.title, destination = Link record.destination } }, string = record.description }
                    :: { style = currStyle, string = string }
                    :: revStyledStrings
                , Nothing
                )

        Link.Image record ->
            Loop
                ( currStyle
                , { style = { currStyle | link = Just { title = Nothing, destination = Image record.src } }, string = record.alt }
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


parse : Parser (List Block.TopLevelInline)
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
        parseHelpNew
        |> map
            (\items ->
                List.map Block.InlineContent items
            )


parseHelpNew : State -> Parser (Step State (List Block.Inline))
parseHelpNew (( inlineStyle, soFar, allFailed ) as state) =
    oneOf
        [ succeed
            (\rawCode ->
                Done [ Block.CodeSpan rawCode ]
            )
            |. token (Token "`" (Parser.Expecting "`"))
            |= getChompedString
                (chompUntil (Token "`" (Parser.Expecting "`")))
        , succeed
            (\rawText ->
                Done
                    [ rawText
                        |> Block.Text
                        |> Block.Italic
                    ]
            )
            |. token (Token "*" (Parser.Expecting "*"))
            |= getChompedString
                (chompUntil (Token "*" (Parser.Expecting "*")))
        , succeed
            (\rawText ->
                Done
                    [ Block.Text rawText
                    ]
            )
            |= (getChompedString <| Advanced.chompUntilEndOr "\n")
        ]


parseHelp : State -> Parser (Step State (List Inline))
parseHelp (( inlineStyle, _, allFailed ) as state) =
    if inlineStyle.isCode then
        Advanced.succeed
            (\chompedString ->
                nextStepWhenFoundCode state chompedString
            )
            |= Advanced.getChompedString (Advanced.chompUntil (Advanced.Token "`" (Parser.Expecting "`")))
            |. token (Token "`" (Parser.Expecting "`"))

    else
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
                        |= getChompedString (chompIf (\_ -> True) (Parser.Expecting ""))
            )
