module Markdown.Parser exposing (parse, deadEndToString)

{-|

@docs parse, deadEndToString

-}

import Dict exposing (Dict)
import Helpers
import HtmlParser exposing (Node(..))
import Markdown.Block as Block exposing (Block, Inline, ListItem, Task)
import Markdown.CodeBlock
import Markdown.Inline as Inline
import Markdown.InlineParser
import Markdown.LinkReferenceDefinition as LinkReferenceDefinition exposing (LinkReferenceDefinition)
import Markdown.ListItem as ListItem
import Markdown.OrderedList
import Markdown.RawBlock as RawBlock exposing (Attribute, RawBlock(..), UnparsedInlines(..))
import Markdown.Table
import Markdown.TableParser as TableParser
import Markdown.UnorderedList
import Parser
import Parser.Advanced as Advanced exposing ((|.), (|=), Nestable(..), Step(..), andThen, chompIf, chompWhile, getChompedString, loop, map, oneOf, problem, succeed, symbol, token)
import Parser.Extra exposing (zeroOrMore)
import ThematicBreak


{-| Try parsing a markdown String into `Markdown.Block.Block`s.

Often you'll want to render these `Block`s directly:

    render renderer markdown =
        markdown
            |> Markdown.parse
            |> Result.mapError deadEndsToString
            |> Result.andThen (\ast -> Markdown.render renderer ast)

    deadEndsToString deadEnds =
        deadEnds
            |> List.map deadEndToString
            |> String.join "\n"

But you can also do a lot with the `Block`s before passing them through:

  - Transform the `Block`s ([example: make each heading one level deeper](TODO))
  - Use the blocks to gather metadata about the markdown document ([example: building a table of contents from `Block`s](TODO))

-}
parse : String -> Result (List (Advanced.DeadEnd String Parser.Problem)) (List Block)
parse input =
    Advanced.run multiParser2 input


deadEndsToString : List (Advanced.DeadEnd String Parser.Problem) -> String
deadEndsToString deadEnds =
    deadEnds
        |> List.map deadEndToString
        |> String.join "\n"


{-| Turn a parsing problem into the default String representation.
-}
deadEndToString : Advanced.DeadEnd String Parser.Problem -> String
deadEndToString deadEnd =
    "Problem at row " ++ String.fromInt deadEnd.row ++ "\n" ++ problemToString deadEnd.problem


problemToString : Parser.Problem -> String
problemToString problem =
    case problem of
        Parser.Expecting string ->
            "Expecting " ++ string

        Parser.ExpectingInt ->
            "Expecting int"

        Parser.ExpectingHex ->
            "Expecting hex"

        Parser.ExpectingOctal ->
            "Expecting octal"

        Parser.ExpectingBinary ->
            "Expecting binary"

        Parser.ExpectingFloat ->
            "Expecting float"

        Parser.ExpectingNumber ->
            "Expecting number"

        Parser.ExpectingVariable ->
            "Expecting variable"

        Parser.ExpectingSymbol string ->
            "Expecting symbol " ++ string

        Parser.ExpectingKeyword string ->
            "Expecting keyword " ++ string

        Parser.ExpectingEnd ->
            "Expecting keyword end"

        Parser.UnexpectedChar ->
            "Unexpected char"

        Parser.Problem problemDescription ->
            problemDescription

        Parser.BadRepeat ->
            "Bad repeat"


type alias Parser a =
    Advanced.Parser String Parser.Problem a


inlineParseHelper : LinkReferenceDefinitions -> UnparsedInlines -> List Block.Inline
inlineParseHelper referencesDict (UnparsedInlines unparsedInlines) =
    let
        referencesDict2 =
            referencesDict
                |> List.map (Tuple.mapSecond (\{ destination, title } -> ( destination, title )))
                |> Dict.fromList

        --
        --myReferences =
        --    Dict.fromList
        --        [ ( "foo", { destination = "/url", title = Just "title" } )
        --        ]
    in
    Markdown.InlineParser.parse referencesDict2 unparsedInlines
        |> List.map mapInline


mapInline : Inline.Inline -> Block.Inline
mapInline inline =
    case inline of
        Inline.Text string ->
            Block.Text string

        Inline.HardLineBreak ->
            Block.HardLineBreak

        Inline.CodeInline string ->
            Block.CodeSpan string

        Inline.Link string maybeString inlines ->
            Block.Link string maybeString (inlines |> List.map mapInline)

        Inline.Image string maybeString inlines ->
            Block.Image string maybeString (inlines |> List.map mapInline)

        Inline.HtmlInline node ->
            node
                |> nodeToRawBlock
                |> Block.HtmlInline

        Inline.Emphasis level inlines ->
            case level of
                1 ->
                    Block.Emphasis (inlines |> List.map mapInline)

                2 ->
                    Block.Strong (inlines |> List.map mapInline)

                _ ->
                    -- TODO fix this
                    Block.Strong (inlines |> List.map mapInline)


levelParser : Int -> Parser Block.HeadingLevel
levelParser level =
    case level of
        1 ->
            succeed Block.H1

        2 ->
            succeed Block.H2

        3 ->
            succeed Block.H3

        4 ->
            succeed Block.H4

        5 ->
            succeed Block.H5

        6 ->
            succeed Block.H6

        _ ->
            problem ("A heading with 1 to 6 #'s, but found " ++ String.fromInt level |> Parser.Expecting)


parseInlines : LinkReferenceDefinitions -> RawBlock -> Parser (Maybe Block)
parseInlines linkReferences rawBlock =
    case rawBlock of
        Heading level unparsedInlines ->
            level
                |> levelParser
                |> andThen
                    (\parsedLevel ->
                        unparsedInlines
                            |> inlineParseHelper linkReferences
                            |> (\styledLine -> just (Block.Heading parsedLevel styledLine))
                    )

        Body unparsedInlines ->
            unparsedInlines
                |> inlineParseHelper linkReferences
                |> (\styledLine -> just (Block.Paragraph styledLine))

        Html html ->
            Block.HtmlBlock html
                |> just

        UnorderedListBlock unparsedItems ->
            unparsedItems
                |> List.map
                    (\unparsedItem ->
                        unparsedItem.body
                            |> parseRawInline linkReferences identity
                            |> Advanced.map
                                (\parsedInlines ->
                                    let
                                        task =
                                            case unparsedItem.task of
                                                Just False ->
                                                    Block.IncompleteTask

                                                Just True ->
                                                    Block.CompletedTask

                                                Nothing ->
                                                    Block.NoTask
                                    in
                                    Block.ListItem task parsedInlines
                                )
                    )
                |> combine
                |> map Block.UnorderedList
                |> map Just

        OrderedListBlock startingIndex unparsedInlines ->
            unparsedInlines
                |> List.map (parseRawInline linkReferences identity)
                |> combine
                |> map (Block.OrderedList startingIndex)
                |> map Just

        CodeBlock codeBlock ->
            Block.CodeBlock codeBlock
                |> just

        ThematicBreak ->
            just Block.ThematicBreak

        BlankLine ->
            succeed Nothing

        BlockQuote rawBlocks ->
            case Advanced.run rawBlockParser rawBlocks of
                Ok value ->
                    parseAllInlines value
                        |> map
                            (\parsedBlocks ->
                                Block.BlockQuote parsedBlocks
                                    |> Just
                            )

                Err error ->
                    Advanced.problem (Parser.Problem (deadEndsToString error))

        IndentedCodeBlock codeBlockBody ->
            Block.CodeBlock { body = codeBlockBody, language = Nothing }
                |> just

        Table (Markdown.Table.Table header rows) ->
            let
                parsedHeader : Parser (List (Markdown.Table.HeaderCell (List Inline)))
                parsedHeader =
                    header
                        |> List.map
                            (\{ label, alignment } ->
                                label
                                    |> UnparsedInlines
                                    |> parseRawInline linkReferences
                                        (\parsedHeaderLabel ->
                                            { label = parsedHeaderLabel
                                            , alignment = alignment
                                            }
                                        )
                            )
                        --|> List.map (parseRawInline linkReferences identity)
                        |> combine

                --|> parseRawInline
                --    linkReferences
                --    (Debug.todo "")
                --    (UnparsedInlines "")
                --header
            in
            parsedHeader
                |> andThen
                    (\headerThing ->
                        Block.Table headerThing []
                            |> just
                    )


just value =
    succeed (Just value)


parseRawInline : LinkReferenceDefinitions -> (List Inline -> a) -> UnparsedInlines -> Advanced.Parser c Parser.Problem a
parseRawInline linkReferences wrap unparsedInlines =
    unparsedInlines
        |> inlineParseHelper linkReferences
        |> (\styledLine -> wrap styledLine)
        |> succeed


plainLine : Parser RawBlock
plainLine =
    succeed
        (\rawLine ->
            rawLine
                |> UnparsedInlines
                |> Body
        )
        |= innerParagraphParser
        |. oneOf
            [ Advanced.chompIf Helpers.isNewline (Parser.Expecting "A single non-newline char.")
            , Advanced.end (Parser.Expecting "End")
            ]


innerParagraphParser =
    getChompedString <|
        succeed ()
            |. Advanced.chompIf (\c -> not <| Helpers.isNewline c) (Parser.Expecting "Not newline.")
            |. Advanced.chompUntilEndOr "\n"


blockQuote : Parser RawBlock
blockQuote =
    succeed BlockQuote
        |. oneOf
            [ symbol (Advanced.Token "   > " (Parser.Expecting "   > "))
            , symbol (Advanced.Token "  > " (Parser.Expecting "  > "))
            , symbol (Advanced.Token " > " (Parser.Expecting " > "))
            , symbol (Advanced.Token "> " (Parser.Expecting "> "))
            , symbol (Advanced.Token "   >" (Parser.Expecting "   >"))
            , symbol (Advanced.Token "  >" (Parser.Expecting "  >"))
            , symbol (Advanced.Token " >" (Parser.Expecting " >"))
            , symbol (Advanced.Token ">" (Parser.Expecting ">"))
            ]
        |= Advanced.getChompedString (Advanced.chompUntilEndOr "\n")
        |. oneOf
            [ Advanced.end (Parser.Problem "Expecting end")
            , chompIf Helpers.isNewline (Parser.Problem "Expecting newline")
            ]


unorderedListBlock : Parser RawBlock
unorderedListBlock =
    Markdown.UnorderedList.parser
        |> map
            (List.map
                (\unparsedListItem ->
                    case unparsedListItem of
                        ListItem.TaskItem completion body ->
                            { body = UnparsedInlines body
                            , task =
                                (case completion of
                                    ListItem.Complete ->
                                        True

                                    ListItem.Incomplete ->
                                        False
                                )
                                    |> Just
                            }

                        ListItem.PlainItem body ->
                            { body = UnparsedInlines body
                            , task = Nothing
                            }
                )
            )
        |> map UnorderedListBlock


orderedListBlock : Maybe RawBlock -> Parser RawBlock
orderedListBlock lastBlock =
    Markdown.OrderedList.parser lastBlock
        |> map (\( startingIndex, unparsedLines ) -> OrderedListBlock startingIndex (List.map UnparsedInlines unparsedLines))


blankLine : Parser RawBlock
blankLine =
    Advanced.backtrackable (chompWhile (\c -> Helpers.isSpaceOrTab c))
        |. token (Advanced.Token "\n" (Parser.Expecting "\\n"))
        |> map (\() -> BlankLine)


htmlParser : Parser RawBlock
htmlParser =
    HtmlParser.html
        |> xmlNodeToHtmlNode


xmlNodeToHtmlNode : Parser Node -> Parser RawBlock
xmlNodeToHtmlNode parser =
    Advanced.andThen
        (\xmlNode ->
            case xmlNode of
                HtmlParser.Text innerText ->
                    Body
                        (UnparsedInlines innerText)
                        |> Advanced.succeed

                HtmlParser.Element tag attributes children ->
                    Advanced.andThen
                        (\parsedChildren ->
                            Block.HtmlElement tag attributes parsedChildren
                                |> RawBlock.Html
                                |> Advanced.succeed
                        )
                        (nodesToBlocksParser children)

                Comment string ->
                    Block.HtmlComment string
                        |> RawBlock.Html
                        |> succeed

                Cdata string ->
                    Block.Cdata string
                        |> RawBlock.Html
                        |> succeed

                ProcessingInstruction string ->
                    Block.ProcessingInstruction string
                        |> RawBlock.Html
                        |> succeed

                Declaration declarationType content ->
                    Block.HtmlDeclaration declarationType content
                        |> RawBlock.Html
                        |> succeed
        )
        parser


textNodeToBlocks : String -> List Block
textNodeToBlocks textNodeValue =
    textNodeValue
        |> Advanced.run multiParser2
        |> Result.withDefault []


nodeToRawBlock : Node -> Block.Html Block
nodeToRawBlock node =
    case node of
        HtmlParser.Text innerText ->
            Block.HtmlComment "TODO this never happens, but use types to drop this case."

        HtmlParser.Element tag attributes children ->
            let
                parsedChildren : List Block
                parsedChildren =
                    children
                        |> List.map
                            (\child ->
                                case child of
                                    HtmlParser.Text text ->
                                        textNodeToBlocks text

                                    _ ->
                                        [ nodeToRawBlock child |> Block.HtmlBlock ]
                            )
                        |> List.concat
            in
            Block.HtmlElement tag
                attributes
                parsedChildren

        Comment string ->
            Block.HtmlComment string

        Cdata string ->
            Block.Cdata string

        ProcessingInstruction string ->
            Block.ProcessingInstruction string

        Declaration declarationType content ->
            Block.HtmlDeclaration declarationType content


nodesToBlocksParser : List Node -> Parser (List Block)
nodesToBlocksParser children =
    children
        |> List.map childToParser
        |> combine
        |> Advanced.map List.concat


combine : List (Parser a) -> Parser (List a)
combine list =
    list
        |> List.foldr
            (\parser listParser ->
                listParser
                    |> Advanced.andThen
                        (\soFar ->
                            parser
                                |> Advanced.map (\a -> a :: soFar)
                        )
            )
            (Advanced.succeed [])


childToParser : Node -> Parser (List Block)
childToParser node =
    case node of
        Element tag attributes children ->
            nodesToBlocksParser children
                |> Advanced.andThen
                    (\childrenAsBlocks ->
                        Advanced.succeed [ Block.HtmlElement tag attributes childrenAsBlocks |> Block.HtmlBlock ]
                    )

        Text innerText ->
            case Advanced.run multiParser2 innerText of
                Ok value ->
                    succeed value

                Err error ->
                    Advanced.problem
                        (Parser.Expecting
                            (error
                                |> List.map deadEndToString
                                |> String.join "\n"
                            )
                        )

        Comment string ->
            succeed [ Block.HtmlComment string |> Block.HtmlBlock ]

        Cdata string ->
            succeed [ Block.Cdata string |> Block.HtmlBlock ]

        ProcessingInstruction string ->
            succeed [ Block.ProcessingInstruction string |> Block.HtmlBlock ]

        Declaration declarationType content ->
            succeed [ Block.HtmlDeclaration declarationType content |> Block.HtmlBlock ]


multiParser2 : Parser (List Block)
multiParser2 =
    rawBlockParser
        |. succeed Advanced.end
        |> andThen parseAllInlines
        -- TODO find a more elegant way to exclude empty blocks for each blank lines
        |> map
            (List.filter
                (\item ->
                    case item of
                        Block.Paragraph [] ->
                            False

                        _ ->
                            True
                )
            )


type alias LinkReferenceDefinitions =
    List ( String, { destination : String, title : Maybe String } )


type alias State =
    { linkReferenceDefinitions : LinkReferenceDefinitions
    , rawBlocks : List RawBlock
    }


updateRawBlocks : State -> List RawBlock -> State
updateRawBlocks state updatedRawBlocks =
    { state | rawBlocks = updatedRawBlocks }


addReference : State -> LinkReferenceDefinition -> State
addReference state linkRef =
    { state | linkReferenceDefinitions = linkRef :: state.linkReferenceDefinitions }


rawBlockParser : Parser State
rawBlockParser =
    loop
        { linkReferenceDefinitions = []
        , rawBlocks = []
        }
        statementsHelp2


parseAllInlines : State -> Parser (List Block)
parseAllInlines state =
    List.foldl (combineBlocks state.linkReferenceDefinitions) (succeed []) state.rawBlocks


combineBlocks : LinkReferenceDefinitions -> RawBlock -> Parser (List Block) -> Parser (List Block)
combineBlocks linkReferences rawBlock soFar =
    soFar
        |> andThen
            (\parsedBlocks ->
                rawBlock
                    |> parseInlines linkReferences
                    |> map
                        (\maybeNewParsedBlock ->
                            case maybeNewParsedBlock of
                                Just newParsedBlock ->
                                    newParsedBlock :: parsedBlocks

                                Nothing ->
                                    parsedBlocks
                        )
            )


statementsHelp2 : State -> Parser (Step State State)
statementsHelp2 revStmts =
    let
        keepLooping parser =
            parser
                |> map
                    (\newRawBlock ->
                        case
                            ( newRawBlock
                            , revStmts.rawBlocks
                            )
                        of
                            ( CodeBlock block1, (CodeBlock block2) :: rest ) ->
                                (CodeBlock
                                    { body = joinStringsPreserveAll block2.body block1.body
                                    , language = Nothing
                                    }
                                    :: rest
                                )
                                    |> updateRawBlocks revStmts
                                    |> Loop

                            ( IndentedCodeBlock block1, (IndentedCodeBlock block2) :: rest ) ->
                                (IndentedCodeBlock (joinStringsPreserveAll block2 block1)
                                    :: rest
                                )
                                    |> updateRawBlocks revStmts
                                    |> Loop

                            ( Body (UnparsedInlines body1), (BlockQuote body2) :: rest ) ->
                                (BlockQuote (joinRawStringsWith "\n" body2 body1)
                                    :: rest
                                )
                                    |> updateRawBlocks revStmts
                                    |> Loop

                            ( BlockQuote body1, (BlockQuote body2) :: rest ) ->
                                (BlockQuote (joinStringsPreserveAll body2 body1)
                                    :: rest
                                )
                                    |> updateRawBlocks revStmts
                                    |> Loop

                            ( Body (UnparsedInlines body1), (Body (UnparsedInlines body2)) :: rest ) ->
                                (Body (UnparsedInlines (joinRawStringsWith "\n" body2 body1))
                                    :: rest
                                )
                                    |> updateRawBlocks revStmts
                                    |> Loop

                            _ ->
                                (newRawBlock :: revStmts.rawBlocks)
                                    |> updateRawBlocks revStmts
                                    |> Loop
                    )

        indentedCodeParser =
            case revStmts.rawBlocks of
                (Body _) :: _ ->
                    oneOf []

                _ ->
                    indentedCodeBlock
                        |> keepLooping
    in
    oneOf
        [ Advanced.end (Parser.Expecting "End") |> map (\() -> Done revStmts)
        , parseAsParagraphInsteadOfHtmlBlock |> keepLooping
        , LinkReferenceDefinition.parser
            |> Advanced.backtrackable
            |> map
                (\linkReference ->
                    linkReference
                        |> addReference revStmts
                        |> Loop
                )
        , blankLine |> keepLooping
        , blockQuote |> keepLooping
        , Markdown.CodeBlock.parser |> Advanced.backtrackable |> map CodeBlock |> keepLooping
        , indentedCodeParser
        , ThematicBreak.parser |> Advanced.backtrackable |> map (\_ -> ThematicBreak) |> keepLooping
        , unorderedListBlock |> keepLooping
        , orderedListBlock (List.head revStmts.rawBlocks) |> keepLooping
        , heading |> Advanced.backtrackable |> keepLooping
        , htmlParser |> keepLooping

        -- TODO re-enable this once the table parser handles rows
        --, TableParser.parser |> Advanced.backtrackable |> map Table |> keepLooping
        , plainLine |> keepLooping
        ]


{-| HTML parsing is intentionally strict in `dillonkearns/elm-markdown`. Paragraphs are supposed to be forgiving.
This function checks to see if something might be an autolink that could be confused with an HTML block because
the line starts with `<`. But it's slightly more lenient, so that things like `<>` that aren't actually parsed as
autolinks are still parsed as paragraphs.
-}
parseAsParagraphInsteadOfHtmlBlock : Parser RawBlock
parseAsParagraphInsteadOfHtmlBlock =
    -- ^<[A-Za-z][A-Za-z0-9.+-]{1,31}:[^<>\x00-\x20]*>
    Advanced.succeed ()
        |. token (Advanced.Token "<" (Parser.Expecting "<"))
        |. thisIsDefinitelyNotAnHtmlTag
        |. endOfLineOrFile
        |> getChompedString
        |> map (\rawLine -> rawLine |> UnparsedInlines |> Body)
        |> Advanced.backtrackable


endOfLineOrFile =
    Advanced.chompUntilEndOr "\n"
        |. oneOf
            [ Advanced.symbol (Advanced.Token "\n" (Parser.ExpectingSymbol "\\n"))
            , Advanced.end (Parser.Expecting "End of input")
            ]


thisIsDefinitelyNotAnHtmlTag : Parser ()
thisIsDefinitelyNotAnHtmlTag =
    oneOf
        [ token (Advanced.Token " " (Parser.Expecting " "))
        , token (Advanced.Token ">" (Parser.Expecting ">"))
        , chompIf Char.isAlpha (Parser.Expecting "Alpha")
            |. chompWhile (\c -> Char.isAlphaNum c || c == '-')
            |. oneOf
                [ token (Advanced.Token ":" (Parser.Expecting ":"))
                , token (Advanced.Token "@" (Parser.Expecting "@"))
                , token (Advanced.Token "\\" (Parser.Expecting "\\"))
                , token (Advanced.Token "+" (Parser.Expecting "+"))
                , token (Advanced.Token "." (Parser.Expecting "."))
                ]
        ]


joinStringsPreserveAll string1 string2 =
    String.concat
        [ string1
        , "\n"
        , string2
        ]


joinRawStringsWith joinWith string1 string2 =
    case ( string1, string2 ) of
        ( "", "" ) ->
            String.concat
                [ string1
                , string2
                ]

        ( "", _ ) ->
            String.concat
                [ string1
                , string2
                ]

        ( _, "" ) ->
            String.concat
                [ string1
                , string2
                ]

        _ ->
            String.concat
                [ string1
                , joinWith
                , string2
                ]


indentedCodeBlock : Parser RawBlock
indentedCodeBlock =
    succeed IndentedCodeBlock
        |. oneOf
            [ Advanced.symbol (Advanced.Token "    " (Parser.ExpectingSymbol "Indentation"))

            --tabs behave as if they were replaced by 4 spaces in places where spaces define structure
            -- see https://spec.commonmark.org/0.29/#tabs
            , Advanced.symbol (Advanced.Token "\t" (Parser.ExpectingSymbol "Indentation"))
            ]
        |= getChompedString (Advanced.chompUntilEndOr "\n")
        |. oneOf
            [ Advanced.symbol (Advanced.Token "\n" (Parser.ExpectingSymbol "\\n"))
            , Advanced.end (Parser.Expecting "End of input")
            ]


heading : Parser RawBlock
heading =
    succeed Heading
        |. symbol (Advanced.Token "#" (Parser.Expecting "#"))
        |= (getChompedString
                (succeed ()
                    |. chompWhile
                        (\c ->
                            case c of
                                '#' ->
                                    True

                                _ ->
                                    False
                        )
                )
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
        |. chompWhile Helpers.isSpacebar
        |= (getChompedString
                (succeed ()
                    |. Advanced.chompUntilEndOr "\n"
                )
                |> Advanced.andThen
                    (\headingText ->
                        headingText
                            |> dropTrailingHashes
                            |> UnparsedInlines
                            |> succeed
                    )
           )


dropTrailingHashes headingString =
    if headingString |> String.endsWith "#" then
        String.dropRight 1 headingString
            |> String.trimRight
            |> dropTrailingHashes

    else
        headingString
