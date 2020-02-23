module Markdown.Parser exposing (parse, deadEndToString)

{-|

@docs parse, deadEndToString

-}

import Dict
import Helpers
import HtmlParser exposing (Node(..))
import Markdown.Block as Block exposing (Block, Inline, ListItem, Task)
import Markdown.CodeBlock
import Markdown.Inline as Inline
import Markdown.InlineParser
import Markdown.ListItem as ListItem
import Markdown.OrderedList
import Markdown.RawBlock exposing (Attribute, RawBlock(..), UnparsedInlines(..))
import Markdown.UnorderedList
import Parser
import Parser.Advanced as Advanced exposing ((|.), (|=), Nestable(..), Step(..), andThen, chompIf, chompWhile, getChompedString, loop, map, oneOf, problem, succeed, symbol, token)
import Parser.Extra exposing (zeroOrMore)


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


inlineParseHelper : UnparsedInlines -> List Block.Inline
inlineParseHelper (UnparsedInlines unparsedInlines) =
    Markdown.InlineParser.parse Dict.empty unparsedInlines
        |> List.map mapInline


mapInline : Inline.Inline String -> Block.Inline
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


parseInlines : RawBlock -> Parser (Maybe Block)
parseInlines rawBlock =
    case rawBlock of
        Heading level unparsedInlines ->
            level
                |> levelParser
                |> andThen
                    (\parsedLevel ->
                        unparsedInlines
                            |> inlineParseHelper
                            |> (\styledLine -> just (Block.Heading parsedLevel styledLine))
                    )

        Body unparsedInlines ->
            unparsedInlines
                |> inlineParseHelper
                |> (\styledLine -> just (Block.Paragraph styledLine))

        Html html ->
            Block.HtmlBlock html
                |> just

        UnorderedListBlock unparsedItems ->
            unparsedItems
                |> List.map
                    (\unparsedItem ->
                        unparsedItem.body
                            |> parseRawInline identity
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
                |> List.map (parseRawInline identity)
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


just value =
    succeed (Just value)


parseRawInline : (List Inline -> a) -> UnparsedInlines -> Advanced.Parser c Parser.Problem a
parseRawInline wrap unparsedInlines =
    unparsedInlines
        |> inlineParseHelper
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
        |. Advanced.backtrackable
            (oneOf
                [ token (Advanced.Token "   " (Parser.Expecting "   "))
                , token (Advanced.Token "  " (Parser.Expecting "  "))
                , token (Advanced.Token " " (Parser.Expecting " "))
                , succeed ()
                ]
            )
        |= innerParagraphParser
        |. oneOf
            [ Advanced.chompIf Helpers.isNewline (Parser.Expecting "A single non-newline char.")
            , Advanced.end (Parser.Expecting "End")
            ]


innerParagraphParser =
    getChompedString <|
        succeed ()
            |. Advanced.chompIf (\c -> not <| Helpers.isSpaceOrTab c && (not <| Helpers.isNewline c)) (Parser.Expecting "Not a space or tab.")
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
    HtmlParser.element
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
                                |> Markdown.RawBlock.Html
                                |> Advanced.succeed
                        )
                        (nodesToBlocksParser children)

                Comment string ->
                    Block.HtmlComment string
                        |> Markdown.RawBlock.Html
                        |> succeed

                Cdata string ->
                    Block.Cdata string
                        |> Markdown.RawBlock.Html
                        |> succeed

                ProcessingInstruction string ->
                    Block.ProcessingInstruction string
                        |> Markdown.RawBlock.Html
                        |> succeed

                Declaration declarationType content ->
                    Block.HtmlDeclaration declarationType content
                        |> Markdown.RawBlock.Html
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


rawBlockParser : Parser (List RawBlock)
rawBlockParser =
    loop [] statementsHelp2


parseAllInlines : List RawBlock -> Parser (List Block)
parseAllInlines rawBlocks =
    List.foldl combineBlocks (succeed []) rawBlocks


combineBlocks : RawBlock -> Parser (List Block) -> Parser (List Block)
combineBlocks rawBlock soFar =
    soFar
        |> andThen
            (\parsedBlocks ->
                rawBlock
                    |> parseInlines
                    |> map
                        (\maybeNewParsedBlock ->
                            case maybeNewParsedBlock of
                                Just newParsedBlock ->
                                    newParsedBlock :: parsedBlocks

                                Nothing ->
                                    parsedBlocks
                        )
            )


statementsHelp2 : List RawBlock -> Parser (Step (List RawBlock) (List RawBlock))
statementsHelp2 revStmts =
    let
        keepLooping parser =
            parser
                |> map
                    (\stmts ->
                        case
                            ( stmts
                            , revStmts
                            )
                        of
                            ( CodeBlock block1, (CodeBlock block2) :: rest ) ->
                                (CodeBlock
                                    { body = joinStringsPreserveIndentation block2.body block1.body
                                    , language = Nothing
                                    }
                                    :: rest
                                )
                                    |> Loop

                            ( Body (UnparsedInlines body1), (BlockQuote body2) :: rest ) ->
                                (BlockQuote (joinRawStringsWith "\n" body2 body1)
                                    :: rest
                                )
                                    |> Loop

                            ( BlockQuote body1, (BlockQuote body2) :: rest ) ->
                                (BlockQuote (joinStringsPreserveAll body2 body1)
                                    :: rest
                                )
                                    |> Loop

                            ( Body (UnparsedInlines body1), (Body (UnparsedInlines body2)) :: rest ) ->
                                Loop
                                    (Body (UnparsedInlines (joinRawStringsWith "\n" body2 body1))
                                        :: rest
                                    )

                            _ ->
                                Loop (stmts :: revStmts)
                    )
    in
    oneOf
        [ Advanced.end (Parser.Expecting "End") |> map (\() -> Done revStmts)
        , blankLine |> keepLooping
        , blockQuote |> keepLooping
        , Markdown.CodeBlock.parser |> map CodeBlock |> keepLooping
        , thematicBreak |> keepLooping
        , unorderedListBlock |> keepLooping
        , orderedListBlock (List.head revStmts) |> keepLooping
        , heading |> keepLooping
        , htmlParser |> Advanced.backtrackable |> keepLooping
        , plainLine |> keepLooping
        , succeed (Done revStmts)
        ]


joinStringsPreserveAll string1 string2 =
    String.concat
        [ string1
        , "\n"
        , string2
        ]


joinStringsPreserveIndentation string1 string2 =
    let
        string1Trimmed =
            String.trimRight string1

        string2Trimmed =
            String.trimRight string2
    in
    String.concat
        [ string1Trimmed
        , "\n"
        , string2Trimmed
        ]


joinRawStringsWith joinWith string1 string2 =
    let
        string1Trimmed =
            String.trim string1

        string2Trimmed =
            String.trim string2
    in
    case ( string1Trimmed, string2Trimmed ) of
        ( "", "" ) ->
            String.concat
                [ string1Trimmed
                , string2Trimmed
                ]

        ( "", _ ) ->
            String.concat
                [ string1Trimmed
                , string2Trimmed
                ]

        ( _, "" ) ->
            String.concat
                [ string1Trimmed
                , string2Trimmed
                ]

        _ ->
            String.concat
                [ string1Trimmed
                , joinWith
                , string2Trimmed
                ]


thematicBreak : Parser RawBlock
thematicBreak =
    succeed ThematicBreak
        |. Advanced.backtrackable
            (oneOf
                [ symbol (Advanced.Token "   " (Parser.Problem "Expecting 3 spaces"))
                , symbol (Advanced.Token "  " (Parser.Problem "Expecting 2 spaces"))
                , symbol (Advanced.Token " " (Parser.Problem "Expecting space"))
                , succeed ()
                ]
            )
        |. oneOf
            [ symbol (Advanced.Token "---" (Parser.Expecting "---"))
                |. chompWhile
                    (\c ->
                        case c of
                            '-' ->
                                True

                            _ ->
                                False
                    )
            , symbol (Advanced.Token "***" (Parser.Expecting "***"))
                |. chompWhile
                    (\c ->
                        case c of
                            '*' ->
                                True

                            _ ->
                                False
                    )
            , symbol (Advanced.Token "___" (Parser.Expecting "___"))
                |. chompWhile
                    (\c ->
                        case c of
                            '_' ->
                                True

                            _ ->
                                False
                    )
            ]
        |. zeroOrMore Helpers.isSpaceOrTab
        |. oneOf
            [ Advanced.end (Parser.Problem "Expecting end")
            , chompIf Helpers.isNewline (Parser.Problem "Expecting newline")
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
