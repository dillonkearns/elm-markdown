module Markdown.Inlines exposing (State, isUninteresting, parse, toString)

import Char
import Helpers
import Markdown.Block as Block
import Markdown.Link as Link exposing (Link)
import Parser
import Parser.Advanced as Advanced exposing (..)


type alias Inline =
    { style : InlineStyle, string : String }


type alias InlineStyle =
    { isBold : Bool
    , isItalic : Bool
    }


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
        --'[' ->
        --    False
        --'!' ->
        --    False
        '*' ->
            False

        '`' ->
            False

        _ ->
            True


type alias State =
    ( InlineStyle, List Block.TopLevelInline, Maybe String )



--nextStepWhenFoundBold : State -> String -> Step State (List Inline)
--nextStepWhenFoundBold ( currStyle, revStyledStrings, _ ) string =
--    Loop
--        ( { currStyle | isBold = not currStyle.isBold }
--        , { style = currStyle, string = string } :: revStyledStrings
--        , Nothing
--        )
--
--
--nextStepWhenFoundLink : Link -> State -> String -> Step State (List Inline)
--nextStepWhenFoundLink link ( currStyle, revStyledStrings, _ ) string =
--    case link of
--        Link.Link record ->
--            Loop
--                ( currStyle
--                , { style = { currStyle | link = Just { title = record.title, destination = Link record.destination } }, string = record.description }
--                    :: { style = currStyle, string = string }
--                    :: revStyledStrings
--                , Nothing
--                )
--
--        Link.Image record ->
--            Loop
--                ( currStyle
--                , { style = { currStyle | link = Just { title = Nothing, destination = Image record.src } }, string = record.alt }
--                    :: { style = currStyle, string = string }
--                    :: revStyledStrings
--                , Nothing
--                )
--
--
--nextStepWhenFoundCode : State -> String -> Step State (List Inline)
--nextStepWhenFoundCode ( currStyle, revStyledStrings, _ ) string =
--    Loop
--        ( { currStyle | isCode = not currStyle.isCode }
--        , { style = currStyle, string = string } :: revStyledStrings
--        , Nothing
--        )
--
--
--nextStepWhenFoundItalic : State -> String -> Step State (List Inline)
--nextStepWhenFoundItalic ( currStyle, revStyledStrings, _ ) string =
--    Loop
--        ( { currStyle | isItalic = not currStyle.isItalic }
--        , { style = currStyle, string = string } :: revStyledStrings
--        , Nothing
--        )
--
--
--nextStepWhenFoundNothing : State -> String -> Step State (List Inline)
--nextStepWhenFoundNothing ( currStyle, revStyledStrings, _ ) string =
--    Done
--        (List.reverse
--            ({ style = currStyle, string = string } :: revStyledStrings)
--            |> List.filter (\thing -> not <| Helpers.isEmptyString thing.string)
--        )
--
--
--nextStepWhenAllFailed : State -> String -> Step State (List Inline)
--nextStepWhenAllFailed ( currStyle, revStyledStrings, _ ) string =
--    Loop
--        ( currStyle, revStyledStrings, Just string )


parse : Parser (List Block.TopLevelInline)
parse =
    loop
        ( { isBold = False
          , isItalic = False
          }
        , []
        , Nothing
        )
        (\state ->
            oneOf
                [ topLevelParseHelp state
                , parseHelpNew state
                ]
        )



--|> map
--    (\items ->
--        List.map Block.InlineContent items
--    )


parseInnerOnly : Parser (List Block.Inline)
parseInnerOnly =
    loop
        ( { isBold = False
          , isItalic = False
          }
        , []
        , Nothing
        )
        parseHelpNew
        |> map
            (\inlines ->
                List.map
                    (\inline ->
                        case inline of
                            Block.InlineContent inlineContent ->
                                inlineContent

                            _ ->
                                Debug.todo "fix this"
                    )
                    inlines
            )


topLevelParseHelp : State -> Parser (Step State (List Block.TopLevelInline))
topLevelParseHelp (( inlineStyle, soFar, allFailed ) as state) =
    succeed
        (\description destination ->
            let
                parsedInnerInlines : List Block.Inline
                parsedInnerInlines =
                    case run parseInnerOnly description of
                        Ok innerInlines ->
                            innerInlines

                        Err error ->
                            Debug.todo ""
            in
            Loop
                ( { isBold = False
                  , isItalic = False
                  }
                , Block.Link { href = destination } parsedInnerInlines
                    :: soFar
                , Nothing
                )
        )
        |. symbol (Token "[" (Parser.ExpectingSymbol "["))
        |= getChompedString
            (chompUntil (Token "]" (Parser.ExpectingSymbol "]")))
        |. symbol (Token "]" (Parser.ExpectingSymbol "]"))
        |. symbol (Token "(" (Parser.ExpectingSymbol "("))
        |= linkDestination
        |. symbol (Token ")" (Parser.ExpectingSymbol ")"))


linkDestination : Parser String
linkDestination =
    oneOf
        [ succeed identity
            |. Advanced.symbol (Advanced.Token "<" (Parser.ExpectingSymbol "<"))
            |= getChompedString
                (chompUntil (Advanced.Token ">" (Parser.ExpectingSymbol ">")))
            |. Advanced.symbol (Advanced.Token ">" (Parser.ExpectingSymbol ">"))
            |> andThen cantContainNewline
        , succeed identity
            |= getChompedString
                (chompUntil (Advanced.Token ")" (Parser.ExpectingSymbol ")")))
            |> andThen cantContainWhitespace
        ]


cantContainNewline : String -> Parser String
cantContainNewline destination =
    if String.contains "\n" destination then
        problem (Parser.Problem "Link destinations can't contain new lines")

    else
        succeed destination


cantContainWhitespace : String -> Parser String
cantContainWhitespace untrimmed =
    let
        destination =
            String.trim untrimmed
    in
    if String.any Helpers.isGfmWhitespace destination then
        problem (Parser.Problem "Link destinations can't contain whitespace, if you would like to include them please wrap your URL with < .. >")

    else
        succeed destination


parseHelpNew : State -> Parser (Step State (List Block.TopLevelInline))
parseHelpNew (( inlineStyle, soFar, allFailed ) as state) =
    let
        --_ =
        --    Debug.log "state" state
        addToLoop thing =
            Loop ( inlineStyle, Block.InlineContent thing :: soFar, Nothing )

        doNothingLoop updatedStyle =
            Loop ( updatedStyle, soFar, Nothing )
    in
    oneOf
        [ succeed identity
            |. end (Parser.Expecting "End of inlines")
            |= succeed
                (Done
                    (List.reverse soFar
                     --({ style = currStyle, string = string } :: revStyledStrings)
                     --|> List.filter (\thing -> not <| Helpers.isEmptyString thing.string)
                    )
                )
        , succeed
            (\escapedChar ->
                Loop ( inlineStyle, (Block.InlineContent <| Block.Text escapedChar) :: soFar, Just escapedChar )
            )
            |. token (Token "\\" (Parser.Expecting "\\"))
            |= getChompedString (chompIf (\_ -> True) (Parser.Expecting "character"))
        , succeed
            (\rawCode ->
                addToLoop <| Block.CodeSpan rawCode
            )
            |. token (Token "``" (Parser.Expecting "``"))
            |= getChompedString
                (chompUntil (Token "``" (Parser.Expecting "``")))
            |. token (Token "``" (Parser.Expecting "``"))
        , succeed
            (\rawCode ->
                if inlineStyle.isItalic then
                    addToLoop <| Block.Italic <| Block.CodeSpan rawCode

                else
                    addToLoop <| Block.CodeSpan rawCode
            )
            |. token (Token "`" (Parser.Expecting "`"))
            |= getChompedString
                (chompUntil (Token "`" (Parser.Expecting "`")))
            |. token (Token "`" (Parser.Expecting "`"))
        , succeed
            (\rawText ->
                rawText
                    |> Block.Text
                    |> Block.Bold
                    |> addToLoop
            )
            |. token (Token "**" (Parser.Expecting "**"))
            |= getChompedString (chompUntil (Token "**" (Parser.Expecting "**")))
            |. token (Token "**" (Parser.Expecting "**"))
        , if inlineStyle.isItalic then
            succeed
                (doNothingLoop { inlineStyle | isItalic = False })
                |. token (Token "*" (Parser.Expecting "*"))

          else
            succeed identity
                |. token (Token "*" (Parser.Expecting "*"))
                |= getChompedString (chompWhile isUninteresting)
                |> andThen
                    (\stringSoFar ->
                        oneOf
                            [ succeed identity
                                |. token (Token "*" (Parser.Expecting "*"))
                                |= (stringSoFar |> Block.Text |> Block.Italic |> addToLoop |> succeed)
                            , succeed ("*" ++ stringSoFar |> Block.Text |> addToLoop)
                                |. end (Parser.Expecting "End of italic")

                            --, succeed
                            --    ("*" ++ stringSoFar |> Block.Text |> addToLoop)
                            , succeed <| Loop ( { inlineStyle | isItalic = True }, soFar, Just stringSoFar )
                            ]
                    )

        --, getChompedString (chompWhile isUninteresting)
        --    |> map
        --        (\rawText ->
        --            rawText |> Block.Text |> Block.Italic |> addToLoop
        --        )
        --]
        , succeed
            (\rawText ->
                case rawText of
                    "" ->
                        Loop
                            ( inlineStyle
                            , soFar
                            , Nothing
                            )

                    "\n" ->
                        Loop
                            ( inlineStyle
                            , soFar
                            , Nothing
                            )

                    _ ->
                        Block.Text rawText |> addToLoop
            )
            |= getChompedString (chompWhile isUninteresting)

        --, succeed
        --    (\rawText ->
        --        Loop ( inlineStyle, Block.Text rawText :: soFar, Nothing )
        --    )
        --    |= (getChompedString <| Advanced.chompUntilEndOr "\n")
        --    |. oneOf
        --        [ Advanced.end (Parser.Problem "Expecting end")
        --        , chompIf Helpers.isNewline (Parser.Problem "Expecting newline")
        --        ]
        ]



--parseHelp : State -> Parser (Step State (List Inline))
--parseHelp (( inlineStyle, _, allFailed ) as state) =
--    if inlineStyle.isCode then
--        Advanced.succeed
--            (\chompedString ->
--                nextStepWhenFoundCode state chompedString
--            )
--            |= Advanced.getChompedString (Advanced.chompUntil (Advanced.Token "`" (Parser.Expecting "`")))
--            |. token (Token "`" (Parser.Expecting "`"))
--
--    else
--        andThen
--            (\chompedString ->
--                oneOf
--                    [ Link.parser
--                        |> map (\link -> nextStepWhenFoundLink link state chompedString)
--                    , map
--                        (\_ -> nextStepWhenFoundCode state chompedString)
--                        (token (Token "`" (Parser.Expecting "`")))
--                    , map
--                        (\_ -> nextStepWhenFoundBold state chompedString)
--                        (token (Token "**" (Parser.Expecting "**")))
--                    , map
--                        (\_ -> nextStepWhenFoundItalic state chompedString)
--                        (token (Token "*" (Parser.Expecting "*")))
--                    , succeed identity
--                        |= succeed (nextStepWhenFoundNothing state chompedString)
--                        |. end (Parser.Expecting "End of inlines")
--                    , succeed (nextStepWhenAllFailed state chompedString)
--                    ]
--            )
--            (case allFailed of
--                Nothing ->
--                    getChompedString (chompWhile isUninteresting)
--
--                Just unhandledString ->
--                    succeed (\chomped -> unhandledString ++ chomped)
--                        |= getChompedString (chompIf (\_ -> True) (Parser.Expecting ""))
--            )
