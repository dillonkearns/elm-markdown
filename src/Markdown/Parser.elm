module Markdown.Parser exposing (parse)

{-|

@docs parse

-}

import Dict
import Helpers
import HtmlParser exposing (Node(..))
import Markdown.Block as Block exposing (Block, Inline)
import Markdown.CodeBlock
import Markdown.Heading as Heading
import Markdown.Helpers exposing (isEven)
import Markdown.Inline as Inline
import Markdown.InlineParser
import Markdown.LinkReferenceDefinition as LinkReferenceDefinition exposing (LinkReferenceDefinition)
import Markdown.ListItem as ListItem
import Markdown.OrderedList
import Markdown.RawBlock as RawBlock exposing (RawBlock(..), SetextLevel(..), UnparsedInlines(..))
import Markdown.Table
import Markdown.TableParser as TableParser
import Markdown.UnorderedList
import Parser
import Parser.Advanced as Advanced exposing ((|.), (|=), Step(..), andThen, chompIf, chompWhile, getChompedString, loop, map, oneOf, succeed, symbol, token)
import Parser.Token as Token
import String exposing (repeat, trim)
import ThematicBreak
import Whitespace


{-| Parse a markdown String into `Markdown.Block.Block`s.

This function always succeeds — any input produces some output, consistent with
how mature markdown parsers handle arbitrary text.

Often you'll want to render these `Block`s directly:

    render renderer markdown =
        markdown
            |> Markdown.Parser.parse
            |> Markdown.Renderer.render renderer

But you can also do a lot with the `Block`s before passing them through:

  - Transform the `Block`s ([example: make each heading one level deeper](https://github.com/dillonkearns/elm-markdown/blob/8fa879e72d33dec98d5cf95af2a8f8cf8c6d5d10/tests/BlockTransformTests.elm#L84-L133))
  - Use the blocks to gather metadata about the markdown document ([example: building a table of contents from `Block`s](https://ellie-app.com/cHB3fRSKVRha1))

-}
parse : String -> List Block
parse input =
    let
        -- first parse the file as raw blocks
        state : State
        state =
            case Advanced.run (rawBlockParser |. Helpers.endOfFile) input of
                Ok v ->
                    v

                Err _ ->
                    -- Defensive fallback: treat entire input as a paragraph
                    { linkReferenceDefinitions = []
                    , rawBlocks = [ OpenBlockOrParagraph (UnparsedInlines input) ]
                    }

        isNotEmptyParagraph : Block -> Bool
        isNotEmptyParagraph block =
            case block of
                Block.Paragraph [] ->
                    False

                _ ->
                    True
    in
    -- then parse the inlines of each raw block
    parseAllInlines state
        |> List.filter isNotEmptyParagraph


type alias Parser a =
    Advanced.Parser String Parser.Problem a


inlineParseHelper : LinkReferenceDefinitions -> UnparsedInlines -> List Block.Inline
inlineParseHelper referencesDict (UnparsedInlines unparsedInlines) =
    let
        mappedReferencesDict : Dict.Dict String.String ( String.String, Maybe String.String )
        mappedReferencesDict =
            referencesDict
                |> List.map (Tuple.mapSecond (\{ destination, title } -> ( destination, title )))
                |> Dict.fromList
    in
    Markdown.InlineParser.parse mappedReferencesDict unparsedInlines
        |> List.map mapInline


mapInline : Inline.Inline -> Block.Inline
mapInline inline =
    -- known-unoptimized-recursion
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
                |> nodeToInlineHtml
                |> Block.HtmlInline

        Inline.Emphasis level inlines ->
            case level of
                1 ->
                    Block.Emphasis (inlines |> List.map mapInline)

                2 ->
                    Block.Strong (inlines |> List.map mapInline)

                _ ->
                    if level |> isEven then
                        Block.Strong [ Inline.Emphasis (level - 2) inlines |> mapInline ]

                    else
                        Block.Emphasis [ Inline.Emphasis (level - 1) inlines |> mapInline ]

        Inline.Strikethrough inlines ->
            Block.Strikethrough (inlines |> List.map mapInline)


toHeading : Int -> Block.HeadingLevel
toHeading level =
    case level of
        1 ->
            Block.H1

        2 ->
            Block.H2

        3 ->
            Block.H3

        4 ->
            Block.H4

        5 ->
            Block.H5

        _ ->
            Block.H6


type InlineResult
    = EmptyBlock
    | ParsedBlock Block


isTightBoolToListDisplay : Bool -> Block.ListSpacing
isTightBoolToListDisplay isTight =
    if isTight then
        Block.Tight

    else
        Block.Loose


parseInlines : LinkReferenceDefinitions -> RawBlock -> InlineResult
parseInlines linkReferences rawBlock =
    case rawBlock of
        Heading level unparsedInlines ->
            unparsedInlines
                |> inlineParseHelper linkReferences
                |> Block.Heading (toHeading level)
                |> ParsedBlock

        OpenBlockOrParagraph unparsedInlines ->
            unparsedInlines
                |> inlineParseHelper linkReferences
                |> Block.Paragraph
                |> ParsedBlock

        Html html _ ->
            Block.HtmlBlock html
                |> ParsedBlock

        UnorderedListBlock tight _ unparsedItems _ ->
            let
                parseItem : Maybe Bool -> List RawBlock -> Block.ListItem Block
                parseItem rawBlockTask rawBlocks =
                    let
                        blocks : List Block
                        blocks =
                            parseAllInlines { linkReferenceDefinitions = linkReferences, rawBlocks = rawBlocks }

                        blocksTask : Block.Task
                        blocksTask =
                            case rawBlockTask of
                                Just False ->
                                    Block.IncompleteTask

                                Just True ->
                                    Block.CompletedTask

                                Nothing ->
                                    Block.NoTask
                    in
                    Block.ListItem blocksTask blocks
            in
            unparsedItems
                |> List.map (\item -> parseItem item.task item.body)
                |> List.reverse
                |> Block.UnorderedList (isTightBoolToListDisplay tight)
                |> ParsedBlock

        OrderedListBlock tight _ _ startingIndex unparsedItems _ ->
            let
                parseItem : List RawBlock -> List Block
                parseItem rawBlocks =
                    parseAllInlines { linkReferenceDefinitions = linkReferences, rawBlocks = rawBlocks }
            in
            unparsedItems
                |> List.map parseItem
                |> List.reverse
                |> Block.OrderedList (isTightBoolToListDisplay tight) startingIndex
                |> ParsedBlock

        RawBlock.CodeBlock codeBlock ->
            Block.CodeBlock codeBlock
                |> ParsedBlock

        RawBlock.ThematicBreak ->
            ParsedBlock Block.ThematicBreak

        BlankLine ->
            EmptyBlock

        RawBlock.BlockQuote _ ->
            EmptyBlock

        ParsedBlockQuote rawBlocks ->
            parseAllInlines { linkReferenceDefinitions = linkReferences, rawBlocks = rawBlocks }
                |> Block.BlockQuote
                |> ParsedBlock

        IndentedCodeBlock codeBlockBody ->
            Block.CodeBlock { body = codeBlockBody, language = Nothing }
                |> ParsedBlock

        RawBlock.Table (Markdown.Table.Table header rows) ->
            Block.Table (parseHeaderInlines linkReferences header) (parseRowInlines linkReferences rows)
                |> ParsedBlock

        TableDelimiter (Markdown.Table.TableDelimiterRow text _) ->
            UnparsedInlines text.raw
                |> inlineParseHelper linkReferences
                |> Block.Paragraph
                |> ParsedBlock

        SetextLine _ raw ->
            UnparsedInlines raw
                |> inlineParseHelper linkReferences
                |> Block.Paragraph
                |> ParsedBlock


parseHeaderInlines : LinkReferenceDefinitions -> List (Markdown.Table.HeaderCell String) -> List (Markdown.Table.HeaderCell (List Inline))
parseHeaderInlines linkReferences header =
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


parseRowInlines : LinkReferenceDefinitions -> List (List String) -> List (List (List Inline))
parseRowInlines linkReferences rows =
    rows
        |> List.map
            (\row ->
                List.map
                    (\column ->
                        column
                            |> UnparsedInlines
                            |> parseRawInline linkReferences identity
                    )
                    row
            )


parseRawInline : LinkReferenceDefinitions -> (List Inline -> a) -> UnparsedInlines -> a
parseRawInline linkReferences wrap unparsedInlines =
    unparsedInlines
        |> inlineParseHelper linkReferences
        |> wrap


openBlockOrParagraphParser : Parser RawBlock
openBlockOrParagraphParser =
    innerParagraphParser
        |. Helpers.lineEndOrEnd


innerParagraphParser : Parser RawBlock
innerParagraphParser =
    Helpers.chompUntilLineEndOrEnd
        |> Advanced.mapChompedString
            (\rawLine _ ->
                rawLine
                    |> UnparsedInlines
                    |> OpenBlockOrParagraph
            )


blockQuoteStarts : List (Parser ())
blockQuoteStarts =
    [ symbol (Advanced.Token ">" (Parser.Expecting ">"))
    , Advanced.backtrackable (symbol Token.space)
        |. oneOf
            [ symbol (Advanced.Token ">" (Parser.Expecting " >"))
            , symbol (Advanced.Token " >" (Parser.Expecting "  >"))
            , symbol (Advanced.Token "  >" (Parser.Expecting "   >"))
            ]
    ]


blockQuote : Parser RawBlock
blockQuote =
    succeed BlockQuote
        |. oneOf blockQuoteStarts
        |. oneOf [ symbol Token.space, succeed () ]
        |= Advanced.getChompedString Helpers.chompUntilLineEndOrEnd
        |. Helpers.lineEndOrEnd


unorderedListBlock : Bool -> Parser RawBlock
unorderedListBlock previousWasBody =
    let
        parseListItem : a -> ListItem.ListItem -> { body : String.String, task : Maybe Bool, marker : a }
        parseListItem listmarker unparsedListItem =
            case unparsedListItem of
                ListItem.TaskItem completion body ->
                    { body = body
                    , task =
                        (case completion of
                            ListItem.Complete ->
                                True

                            ListItem.Incomplete ->
                                False
                        )
                            |> Just
                    , marker = listmarker
                    }

                ListItem.PlainItem body ->
                    { body = body
                    , task = Nothing
                    , marker = listmarker
                    }

                ListItem.EmptyItem ->
                    { body = "" --++ Debug.toString (Advanced.run getIndent "     1   2")
                    , task = Nothing
                    , marker = listmarker
                    }
    in
    Markdown.UnorderedList.parser previousWasBody
        |> map
            (\( listmarker, intended, unparsedListItem ) ->
                UnorderedListBlock True
                    intended
                    []
                    (parseListItem listmarker unparsedListItem)
            )


orderedListBlock : Bool -> Parser RawBlock
orderedListBlock previousWasBody =
    Markdown.OrderedList.parser previousWasBody
        |> map (\item -> OrderedListBlock True item.intended item.marker item.order [] item.body)


blankLine : Parser RawBlock
blankLine =
    Advanced.backtrackable (chompWhile Whitespace.isSpaceOrTab)
        |. Whitespace.lineEnd
        |> map (\_ -> BlankLine)


htmlParser : Parser RawBlock
htmlParser =
    (HtmlParser.html |. chompWhile Whitespace.isSpaceOrTab)
        |> Advanced.mapChompedString (\raw node -> ( raw, node ))
        |> Advanced.andThen (\( raw, node ) -> xmlNodeToHtmlNode raw node)


multiLineHtmlParser : Parser RawBlock
multiLineHtmlParser =
    HtmlParser.html
        |> Advanced.mapChompedString (\raw node -> ( raw, node ))
        |> Advanced.andThen
            (\( raw, node ) ->
                if String.contains "\n" raw then
                    xmlNodeToHtmlNode raw node

                else
                    Advanced.problem (Parser.Expecting "multi-line HTML")
            )
        |> Advanced.backtrackable


xmlNodeToHtmlNode : String -> Node -> Parser RawBlock
xmlNodeToHtmlNode raw xmlNode =
    case xmlNode of
        HtmlParser.Text innerText ->
            OpenBlockOrParagraph (UnparsedInlines innerText)
                |> succeed

        HtmlParser.Element tag attributes children rawBody ->
            Block.HtmlElement tag attributes (nodesToBlocks children) rawBody
                |> (\html -> RawBlock.Html html raw)
                |> succeed

        Comment string ->
            Block.HtmlComment string
                |> (\html -> RawBlock.Html html raw)
                |> succeed

        Cdata string ->
            Block.Cdata string
                |> (\html -> RawBlock.Html html raw)
                |> succeed

        ProcessingInstruction string ->
            Block.ProcessingInstruction string
                |> (\html -> RawBlock.Html html raw)
                |> succeed

        Declaration declarationType content ->
            Block.HtmlDeclaration declarationType content
                |> (\html -> RawBlock.Html html raw)
                |> succeed

        HtmlParser.ClosingTag _ ->
            Advanced.problem (Parser.Expecting "")


nodeToInlineHtml : Node -> Block.Html Inline
nodeToInlineHtml node =
    case node of
        HtmlParser.Text _ ->
            Block.HtmlComment "TODO this never happens, but use types to drop this case."

        HtmlParser.Element tag attributes children rawBody ->
            let
                parseChild : Node -> List Inline
                parseChild child =
                    case child of
                        HtmlParser.Text text ->
                            textNodeToInlines text

                        HtmlParser.ClosingTag tagName ->
                            [ Block.Text ("</" ++ tagName ++ ">") ]

                        _ ->
                            [ nodeToInlineHtml child |> Block.HtmlInline ]
            in
            Block.HtmlElement tag
                attributes
                (List.concatMap parseChild children)
                rawBody

        Comment string ->
            Block.HtmlComment string

        Cdata string ->
            Block.Cdata string

        ProcessingInstruction string ->
            Block.ProcessingInstruction string

        Declaration declarationType content ->
            Block.HtmlDeclaration declarationType content

        HtmlParser.ClosingTag _ ->
            Block.HtmlComment "TODO this never happens, but use types to drop this case."


textNodeToInlines : String -> List Inline
textNodeToInlines textNodeValue =
    -- Parse text content as inline markdown
    let
        mappedReferencesDict : Dict.Dict String ( String, Maybe String )
        mappedReferencesDict =
            Dict.empty
    in
    Markdown.InlineParser.parse mappedReferencesDict textNodeValue
        |> List.map mapInline


nodesToBlocks : List Node -> List Block
nodesToBlocks children =
    nodesToBlocksHelp children []


nodesToBlocksHelp : List Node -> List Block -> List Block
nodesToBlocksHelp remaining soFar =
    case remaining of
        node :: rest ->
            nodesToBlocksHelp rest (childToBlocks node soFar)

        [] ->
            List.reverse soFar


{-| Add the blocks from this node to the passed-in list of blocks
-}
childToBlocks : Node -> List Block -> List Block
childToBlocks node blocks =
    case node of
        Element tag attributes children rawBody ->
            let
                block : Block
                block =
                    Block.HtmlElement tag attributes (nodesToBlocks children) rawBody
                        |> Block.HtmlBlock
            in
            block :: blocks

        Text innerText ->
            List.reverse (parse innerText) ++ blocks

        Comment string ->
            Block.HtmlBlock (Block.HtmlComment string) :: blocks

        Cdata string ->
            Block.HtmlBlock (Block.Cdata string) :: blocks

        ProcessingInstruction string ->
            Block.HtmlBlock (Block.ProcessingInstruction string) :: blocks

        Declaration declarationType content ->
            Block.HtmlBlock (Block.HtmlDeclaration declarationType content) :: blocks

        HtmlParser.ClosingTag _ ->
            blocks


type alias LinkReferenceDefinitions =
    List ( String, { destination : String, title : Maybe String } )


type alias State =
    { linkReferenceDefinitions : LinkReferenceDefinitions
    , rawBlocks : List RawBlock
    }


addReference : State -> LinkReferenceDefinition -> State
addReference state linkRef =
    { linkReferenceDefinitions = linkRef :: state.linkReferenceDefinitions
    , rawBlocks = state.rawBlocks
    }


runRawBlockParserInfallible : String -> State
runRawBlockParserInfallible input =
    case Advanced.run rawBlockParser input of
        Ok state ->
            state

        Err _ ->
            { linkReferenceDefinitions = []
            , rawBlocks = [ OpenBlockOrParagraph (UnparsedInlines input) ]
            }


rawBlockParser : Parser State
rawBlockParser =
    loop
        { linkReferenceDefinitions = []
        , rawBlocks = []
        }
        stepRawBlock
        |> andThen completeBlocks


parseAllInlines : State -> List Block
parseAllInlines state =
    parseAllInlinesHelp state state.rawBlocks []


parseAllInlinesHelp : State -> List RawBlock -> List Block -> List Block
parseAllInlinesHelp state rawBlocks parsedBlocks =
    case rawBlocks of
        rawBlock :: rest ->
            case parseInlines state.linkReferenceDefinitions rawBlock of
                ParsedBlock newParsedBlock ->
                    parseAllInlinesHelp state rest (newParsedBlock :: parsedBlocks)

                EmptyBlock ->
                    -- ignore empty blocks
                    parseAllInlinesHelp state rest parsedBlocks

        [] ->
            parsedBlocks


endWithOpenBlockOrParagraph : RawBlock -> Bool
endWithOpenBlockOrParagraph block =
    case block of
        OpenBlockOrParagraph (UnparsedInlines str) ->
            not (String.endsWith str "\n")

        ParsedBlockQuote blocks ->
            case blocks of
                last :: _ ->
                    endWithOpenBlockOrParagraph last

                _ ->
                    False

        OrderedListBlock _ _ _ _ blockslist _ ->
            case blockslist of
                blocks :: _ ->
                    case blocks of
                        last :: _ ->
                            endWithOpenBlockOrParagraph last

                        _ ->
                            False

                _ ->
                    False

        Heading _ _ ->
            True

        _ ->
            False


completeOrMergeBlocks : State -> RawBlock -> Parser State
completeOrMergeBlocks state newRawBlock =
    case
        ( newRawBlock
        , state.rawBlocks
        )
    of
        ( CodeBlock block1, (CodeBlock block2) :: rest ) ->
            succeed
                { linkReferenceDefinitions = state.linkReferenceDefinitions
                , rawBlocks =
                    CodeBlock
                        { body = joinStringsPreserveAll block2.body block1.body
                        , language = Nothing
                        }
                        :: rest
                }

        ( IndentedCodeBlock block1, (IndentedCodeBlock block2) :: rest ) ->
            succeed
                { linkReferenceDefinitions = state.linkReferenceDefinitions
                , rawBlocks =
                    IndentedCodeBlock (joinStringsPreserveAll block2 block1)
                        :: rest
                }

        ( BlankLine, (IndentedCodeBlock block) :: rest ) ->
            succeed
                { linkReferenceDefinitions = state.linkReferenceDefinitions
                , rawBlocks =
                    IndentedCodeBlock (joinStringsPreserveAll block "\n")
                        :: rest
                }

        ( _, (BlockQuote body2) :: rest ) ->
            case newRawBlock of
                BlockQuote body1 ->
                    succeed
                        { linkReferenceDefinitions = state.linkReferenceDefinitions
                        , rawBlocks =
                            BlockQuote (joinStringsPreserveAll body2 body1)
                                :: rest
                        }

                OpenBlockOrParagraph (UnparsedInlines body1) ->
                    let
                        value : State
                        value =
                            runRawBlockParserInfallible body2
                    in
                    case value.rawBlocks of
                        last :: _ ->
                            if endWithOpenBlockOrParagraph last && not (String.endsWith "\n" body2) then
                                succeed
                                    { linkReferenceDefinitions = state.linkReferenceDefinitions
                                    , rawBlocks =
                                        BlockQuote (joinStringsPreserveAll body2 body1)
                                            :: rest
                                    }

                            else
                                succeed
                                    { linkReferenceDefinitions = state.linkReferenceDefinitions ++ value.linkReferenceDefinitions
                                    , rawBlocks = newRawBlock :: (value.rawBlocks |> ParsedBlockQuote) :: rest
                                    }

                        _ ->
                            succeed
                                { linkReferenceDefinitions = state.linkReferenceDefinitions ++ value.linkReferenceDefinitions
                                , rawBlocks = newRawBlock :: (value.rawBlocks |> ParsedBlockQuote) :: rest
                                }

                IndentedCodeBlock body1 ->
                    let
                        value : State
                        value =
                            runRawBlockParserInfallible body2
                    in
                    case value.rawBlocks of
                        (OpenBlockOrParagraph _) :: _ ->
                            succeed
                                { linkReferenceDefinitions = state.linkReferenceDefinitions
                                , rawBlocks =
                                    BlockQuote (joinRawStringsWith " " body2 body1)
                                        :: rest
                                }

                        _ ->
                            succeed
                                { linkReferenceDefinitions = state.linkReferenceDefinitions ++ value.linkReferenceDefinitions
                                , rawBlocks = newRawBlock :: (value.rawBlocks |> ParsedBlockQuote) :: rest
                                }

                _ ->
                    let
                        value : State
                        value =
                            runRawBlockParserInfallible body2
                    in
                    succeed
                        { linkReferenceDefinitions = state.linkReferenceDefinitions ++ value.linkReferenceDefinitions
                        , rawBlocks = newRawBlock :: (value.rawBlocks |> ParsedBlockQuote) :: rest
                        }

        ( _, (UnorderedListBlock tight intended1 closeListItems2 openListItem2) :: rest ) ->
            case newRawBlock of
                UnorderedListBlock _ intended2 _ openListItem1 ->
                    let
                        value : State
                        value =
                            runRawBlockParserInfallible openListItem2.body
                    in
                    if openListItem2.marker == openListItem1.marker then
                        if List.member BlankLine value.rawBlocks then
                            succeed
                                { linkReferenceDefinitions = state.linkReferenceDefinitions ++ value.linkReferenceDefinitions
                                , rawBlocks = UnorderedListBlock False intended2 ({ task = openListItem2.task, body = value.rawBlocks } :: closeListItems2) openListItem1 :: rest
                                }

                        else
                            succeed
                                { linkReferenceDefinitions = state.linkReferenceDefinitions ++ value.linkReferenceDefinitions
                                , rawBlocks = UnorderedListBlock tight intended2 ({ task = openListItem2.task, body = value.rawBlocks } :: closeListItems2) openListItem1 :: rest
                                }

                    else
                        let
                            tight2 : Bool
                            tight2 =
                                if List.member BlankLine value.rawBlocks then
                                    False

                                else
                                    tight
                        in
                        succeed
                            { linkReferenceDefinitions = state.linkReferenceDefinitions ++ value.linkReferenceDefinitions
                            , rawBlocks = newRawBlock :: UnorderedListBlock tight2 intended1 ({ task = openListItem2.task, body = value.rawBlocks } :: closeListItems2) openListItem1 :: rest
                            }

                OpenBlockOrParagraph (UnparsedInlines body1) ->
                    succeed
                        { linkReferenceDefinitions = state.linkReferenceDefinitions
                        , rawBlocks =
                            UnorderedListBlock tight intended1 closeListItems2 { openListItem2 | body = joinRawStringsWith "\n" openListItem2.body body1 }
                                :: rest
                        }

                _ ->
                    let
                        value : State
                        value =
                            runRawBlockParserInfallible openListItem2.body

                        tight2 : Bool
                        tight2 =
                            if List.member BlankLine value.rawBlocks then
                                False

                            else
                                tight
                    in
                    succeed
                        { linkReferenceDefinitions = state.linkReferenceDefinitions ++ value.linkReferenceDefinitions
                        , rawBlocks = newRawBlock :: UnorderedListBlock tight2 intended1 ({ task = openListItem2.task, body = value.rawBlocks } :: closeListItems2) openListItem2 :: rest
                        }

        -- OrderedListBlock Bool Int OrderedListMarker Int (List (List RawBlock)) String
        -- (\item -> OrderedListBlock True item.intended item.marker item.order [] item.body)
        ( _, (OrderedListBlock tight intended1 marker order closeListItems2 openListItem2) :: rest ) ->
            case newRawBlock of
                OrderedListBlock _ intended2 marker2 _ _ openListItem1 ->
                    let
                        value : State
                        value =
                            runRawBlockParserInfallible openListItem2

                        tight2 : Bool
                        tight2 =
                            if List.member BlankLine value.rawBlocks then
                                False

                            else
                                tight
                    in
                    if marker == marker2 then
                        succeed
                            { linkReferenceDefinitions = state.linkReferenceDefinitions ++ value.linkReferenceDefinitions
                            , rawBlocks = OrderedListBlock tight2 intended2 marker order (value.rawBlocks :: closeListItems2) openListItem1 :: rest
                            }

                    else
                        succeed
                            { linkReferenceDefinitions = state.linkReferenceDefinitions ++ value.linkReferenceDefinitions
                            , rawBlocks = newRawBlock :: OrderedListBlock tight2 intended1 marker order (value.rawBlocks :: closeListItems2) openListItem2 :: rest
                            }

                OpenBlockOrParagraph (UnparsedInlines body1) ->
                    succeed
                        { linkReferenceDefinitions = state.linkReferenceDefinitions
                        , rawBlocks =
                            OrderedListBlock tight intended1 marker order closeListItems2 (openListItem2 ++ "\n" ++ body1)
                                :: rest
                        }

                _ ->
                    let
                        value : State
                        value =
                            runRawBlockParserInfallible openListItem2

                        tight2 : Bool
                        tight2 =
                            if List.member BlankLine value.rawBlocks then
                                False

                            else
                                tight
                    in
                    succeed
                        { linkReferenceDefinitions = state.linkReferenceDefinitions ++ value.linkReferenceDefinitions
                        , rawBlocks = newRawBlock :: OrderedListBlock tight2 intended1 marker order (value.rawBlocks :: closeListItems2) openListItem2 :: rest
                        }

        ( OpenBlockOrParagraph (UnparsedInlines body1), (OpenBlockOrParagraph (UnparsedInlines body2)) :: rest ) ->
            succeed
                { linkReferenceDefinitions = state.linkReferenceDefinitions
                , rawBlocks =
                    OpenBlockOrParagraph (UnparsedInlines (joinRawStringsWith "\n" body2 body1))
                        :: rest
                }

        ( SetextLine LevelOne _, (OpenBlockOrParagraph unparsedInlines) :: rest ) ->
            succeed
                { linkReferenceDefinitions = state.linkReferenceDefinitions
                , rawBlocks =
                    Heading 1 unparsedInlines
                        :: rest
                }

        ( SetextLine LevelTwo _, (OpenBlockOrParagraph unparsedInlines) :: rest ) ->
            succeed
                { linkReferenceDefinitions = state.linkReferenceDefinitions
                , rawBlocks =
                    Heading 2 unparsedInlines
                        :: rest
                }

        ( TableDelimiter (Markdown.Table.TableDelimiterRow text alignments), (OpenBlockOrParagraph (UnparsedInlines rawHeaders)) :: rest ) ->
            case TableParser.parseHeader (Markdown.Table.TableDelimiterRow text alignments) rawHeaders of
                Ok (Markdown.Table.TableHeader headers) ->
                    succeed
                        { linkReferenceDefinitions = state.linkReferenceDefinitions
                        , rawBlocks = Table (Markdown.Table.Table headers []) :: rest
                        }

                Err _ ->
                    succeed
                        { linkReferenceDefinitions = state.linkReferenceDefinitions
                        , rawBlocks =
                            OpenBlockOrParagraph (UnparsedInlines (joinRawStringsWith "\n" rawHeaders text.raw))
                                :: rest
                        }

        ( Table updatedTable, (Table _) :: rest ) ->
            succeed
                { linkReferenceDefinitions = state.linkReferenceDefinitions
                , rawBlocks = Table updatedTable :: rest
                }

        ( _, BlankLine :: (OrderedListBlock tight intended1 marker order closeListItems2 openListItem2) :: rest ) ->
            let
                value : State
                value =
                    runRawBlockParserInfallible openListItem2
            in
            case newRawBlock of
                OrderedListBlock _ intended2 _ _ _ openListItem ->
                    succeed
                        { linkReferenceDefinitions = state.linkReferenceDefinitions ++ value.linkReferenceDefinitions
                        , rawBlocks = OrderedListBlock False intended2 marker order (value.rawBlocks :: closeListItems2) openListItem :: rest
                        }

                _ ->
                    succeed
                        { linkReferenceDefinitions = state.linkReferenceDefinitions ++ value.linkReferenceDefinitions
                        , rawBlocks = newRawBlock :: BlankLine :: OrderedListBlock tight intended1 marker order (value.rawBlocks :: closeListItems2) openListItem2 :: rest
                        }

        ( _, BlankLine :: (UnorderedListBlock tight intended1 closeListItems2 openListItem2) :: rest ) ->
            let
                value : State
                value =
                    runRawBlockParserInfallible openListItem2.body
            in
            case newRawBlock of
                UnorderedListBlock _ _ _ openListItem ->
                    succeed
                        { linkReferenceDefinitions = state.linkReferenceDefinitions ++ value.linkReferenceDefinitions
                        , rawBlocks = UnorderedListBlock False intended1 ({ task = openListItem2.task, body = value.rawBlocks } :: closeListItems2) openListItem :: rest
                        }

                _ ->
                    succeed
                        { linkReferenceDefinitions = state.linkReferenceDefinitions ++ value.linkReferenceDefinitions
                        , rawBlocks = newRawBlock :: BlankLine :: UnorderedListBlock tight intended1 ({ task = openListItem2.task, body = value.rawBlocks } :: closeListItems2) openListItem2 :: rest
                        }

        -- Single-line HTML on same line as following text (htmlParser doesn't consume \n,
        -- so no BlankLine between them). E.g. `<foo>bar</foo>` with ` text` remaining on same line.
        ( OpenBlockOrParagraph (UnparsedInlines body1), (Html _ rawHtmlText) :: rest ) ->
            if not (String.contains "\n" rawHtmlText) then
                succeed
                    { linkReferenceDefinitions = state.linkReferenceDefinitions
                    , rawBlocks =
                        OpenBlockOrParagraph (UnparsedInlines (rawHtmlText ++ body1))
                            :: rest
                    }

            else
                succeed
                    { linkReferenceDefinitions = state.linkReferenceDefinitions
                    , rawBlocks = newRawBlock :: state.rawBlocks
                    }

        -- Single-line HTML followed by text on next line. The \n after the HTML tag
        -- is consumed as BlankLine by the block parser, so we see [BlankLine, Html ...].
        -- For single-line HTML, merge into a paragraph so inline parser handles the tag.
        ( OpenBlockOrParagraph (UnparsedInlines body1), BlankLine :: (Html _ rawHtmlText) :: rest ) ->
            if not (String.contains "\n" rawHtmlText) then
                succeed
                    { linkReferenceDefinitions = state.linkReferenceDefinitions
                    , rawBlocks =
                        OpenBlockOrParagraph (UnparsedInlines (joinRawStringsWith "\n" rawHtmlText body1))
                            :: rest
                    }

            else
                succeed
                    { linkReferenceDefinitions = state.linkReferenceDefinitions
                    , rawBlocks = newRawBlock :: state.rawBlocks
                    }

        _ ->
            succeed
                { linkReferenceDefinitions = state.linkReferenceDefinitions
                , rawBlocks = newRawBlock :: state.rawBlocks
                }



-- RAW BLOCK PARSER


stepRawBlock : State -> Parser (Step State State)
stepRawBlock revStmts =
    -- Some blocks can't immediately follow a body
    oneOf
        [ Helpers.endOfFile
            |> map (\_ -> Done revStmts)
        , LinkReferenceDefinition.parser
            |> Advanced.backtrackable
            |> map (\reference -> Loop (addReference revStmts reference))
        , case revStmts.rawBlocks of
            (OpenBlockOrParagraph _) :: _ ->
                mergeableBlockAfterOpenBlockOrParagraphParser
                    |> andThen (completeOrMergeBlocks revStmts)
                    |> map (\block -> Loop block)

            (Table table) :: _ ->
                oneOf
                    [ mergeableBlockNotAfterOpenBlockOrParagraphParser
                    , tableRowIfTableStarted table
                    ]
                    |> andThen (completeOrMergeBlocks revStmts)
                    |> map (\block -> Loop block)

            (UnorderedListBlock tight intended closeListItems openListItem) :: rest ->
                let
                    completeOrMergeUnorderedListBlock : { a | rawBlocks : List RawBlock } -> String.String -> { a | rawBlocks : List RawBlock }
                    completeOrMergeUnorderedListBlock state newString =
                        { state
                            | rawBlocks =
                                ({ openListItem | body = joinRawStringsWith "\n" openListItem.body newString }
                                    |> UnorderedListBlock tight intended closeListItems
                                )
                                    :: rest
                        }

                    completeOrMergeUnorderedListBlockBlankLine : { a | rawBlocks : List RawBlock } -> String.String -> { a | rawBlocks : List RawBlock }
                    completeOrMergeUnorderedListBlockBlankLine state newString =
                        { state
                            | rawBlocks =
                                BlankLine
                                    :: ({ openListItem | body = joinRawStringsWith "" openListItem.body newString }
                                            |> UnorderedListBlock tight intended closeListItems
                                       )
                                    :: rest
                        }
                in
                oneOf
                    [ blankLine
                        |> map (\_ -> completeOrMergeUnorderedListBlockBlankLine revStmts "\n")
                        |> map (\block -> Loop block)
                    , succeed identity
                        |. Advanced.symbol (Advanced.Token (repeat intended " ") (Parser.ExpectingSymbol "Indentation"))
                        |= getChompedString Helpers.chompUntilLineEndOrEnd
                        |. Helpers.lineEndOrEnd
                        |> map (completeOrMergeUnorderedListBlock revStmts)
                        |> map (\block -> Loop block)
                    , mergeableBlockAfterList
                        |> andThen (completeOrMergeBlocks revStmts)
                        |> map (\block -> Loop block)
                    ]

            BlankLine :: (UnorderedListBlock tight intended closeListItems openListItem) :: rest ->
                let
                    completeOrMergeUnorderedListBlock : { a | rawBlocks : List RawBlock } -> String.String -> { a | rawBlocks : List RawBlock }
                    completeOrMergeUnorderedListBlock state newString =
                        { state
                            | rawBlocks =
                                ({ openListItem | body = joinRawStringsWith "\n" openListItem.body newString }
                                    |> UnorderedListBlock tight intended closeListItems
                                )
                                    :: rest
                        }

                    completeOrMergeUnorderedListBlockBlankLine : { a | rawBlocks : List RawBlock } -> String.String -> { a | rawBlocks : List RawBlock }
                    completeOrMergeUnorderedListBlockBlankLine state newString =
                        { state
                            | rawBlocks =
                                BlankLine
                                    :: ({ openListItem | body = joinRawStringsWith "" openListItem.body newString }
                                            |> UnorderedListBlock tight intended closeListItems
                                       )
                                    :: rest
                        }
                in
                if trim openListItem.body == "" then
                    mergeableBlockNotAfterOpenBlockOrParagraphParser
                        |> andThen (completeOrMergeBlocks revStmts)
                        |> map (\block -> Loop block)

                else
                    oneOf
                        [ blankLine
                            |> map (\_ -> completeOrMergeUnorderedListBlockBlankLine revStmts "\n")
                            |> map (\block -> Loop block)
                        , succeed identity
                            |. Advanced.symbol (Advanced.Token (repeat intended " ") (Parser.ExpectingSymbol "Indentation"))
                            |= getChompedString Helpers.chompUntilLineEndOrEnd
                            |. Helpers.lineEndOrEnd
                            |> map (completeOrMergeUnorderedListBlock revStmts)
                            |> map (\block -> Loop block)
                        , mergeableBlockNotAfterOpenBlockOrParagraphParser
                            |> andThen (completeOrMergeBlocks revStmts)
                            |> map (\block -> Loop block)
                        ]

            (OrderedListBlock tight intended marker order closeListItems openListItem) :: rest ->
                let
                    completeOrMergeUnorderedListBlock : { a | rawBlocks : List RawBlock } -> String.String -> { a | rawBlocks : List RawBlock }
                    completeOrMergeUnorderedListBlock state newString =
                        { state
                            | rawBlocks =
                                OrderedListBlock tight intended marker order closeListItems (openListItem ++ "\n" ++ newString)
                                    :: rest
                        }

                    completeOrMergeUnorderedListBlockBlankLine : { a | rawBlocks : List RawBlock } -> String.String -> { a | rawBlocks : List RawBlock }
                    completeOrMergeUnorderedListBlockBlankLine state newString =
                        { state
                            | rawBlocks =
                                BlankLine
                                    :: OrderedListBlock tight intended marker order closeListItems (openListItem ++ "\n" ++ newString)
                                    :: rest
                        }
                in
                oneOf
                    [ blankLine
                        |> map (\_ -> completeOrMergeUnorderedListBlockBlankLine revStmts "\n")
                        |> map (\block -> Loop block)
                    , succeed identity
                        |. Advanced.symbol (Advanced.Token (repeat intended " ") (Parser.ExpectingSymbol "Indentation"))
                        |= getChompedString Helpers.chompUntilLineEndOrEnd
                        |. Helpers.lineEndOrEnd
                        |> map (completeOrMergeUnorderedListBlock revStmts)
                        |> map (\block -> Loop block)
                    , mergeableBlockAfterList
                        |> andThen (completeOrMergeBlocks revStmts)
                        |> map (\block -> Loop block)
                    ]

            BlankLine :: (OrderedListBlock tight intended marker order closeListItems openListItem) :: rest ->
                let
                    completeOrMergeUnorderedListBlock : { a | rawBlocks : List RawBlock } -> String.String -> { a | rawBlocks : List RawBlock }
                    completeOrMergeUnorderedListBlock state newString =
                        { state
                            | rawBlocks =
                                OrderedListBlock tight intended marker order closeListItems (openListItem ++ "\n" ++ newString)
                                    :: rest
                        }

                    completeOrMergeUnorderedListBlockBlankLine : { a | rawBlocks : List RawBlock } -> String.String -> { a | rawBlocks : List RawBlock }
                    completeOrMergeUnorderedListBlockBlankLine state newString =
                        { state
                            | rawBlocks =
                                BlankLine
                                    :: OrderedListBlock tight intended marker order closeListItems (openListItem ++ "\n" ++ newString)
                                    :: rest
                        }
                in
                if trim openListItem == "" then
                    mergeableBlockNotAfterOpenBlockOrParagraphParser
                        |> andThen (completeOrMergeBlocks revStmts)
                        |> map (\block -> Loop block)

                else
                    oneOf
                        [ blankLine
                            |> map (\_ -> completeOrMergeUnorderedListBlockBlankLine revStmts "\n")
                            |> map (\block -> Loop block)
                        , succeed identity
                            |. Advanced.symbol (Advanced.Token (repeat intended " ") (Parser.ExpectingSymbol "Indentation"))
                            |= getChompedString Helpers.chompUntilLineEndOrEnd
                            |. Helpers.lineEndOrEnd
                            |> map (completeOrMergeUnorderedListBlock revStmts)
                            |> map (\block -> Loop block)
                        , mergeableBlockNotAfterOpenBlockOrParagraphParser
                            |> andThen (completeOrMergeBlocks revStmts)
                            |> map (\block -> Loop block)
                        ]

            _ ->
                mergeableBlockNotAfterOpenBlockOrParagraphParser
                    |> andThen (completeOrMergeBlocks revStmts)
                    |> map (\block -> Loop block)
        , openBlockOrParagraphParser
            |> andThen (completeOrMergeBlocks revStmts)
            |> map (\block -> Loop block)
        ]


completeBlocks :
    State
    -> Parser State
completeBlocks state =
    case state.rawBlocks of
        (BlockQuote body2) :: rest ->
            let
                value : State
                value =
                    runRawBlockParserInfallible body2
            in
            succeed
                { linkReferenceDefinitions = state.linkReferenceDefinitions ++ value.linkReferenceDefinitions
                , rawBlocks = (value.rawBlocks |> ParsedBlockQuote) :: rest
                }

        (UnorderedListBlock tight intended closeListItems openListItem) :: rest ->
            let
                value : State
                value =
                    runRawBlockParserInfallible openListItem.body

                tight2 : Bool
                tight2 =
                    if List.member BlankLine value.rawBlocks then
                        False

                    else
                        tight
            in
            succeed
                { linkReferenceDefinitions = state.linkReferenceDefinitions ++ value.linkReferenceDefinitions
                , rawBlocks = UnorderedListBlock tight2 intended ({ task = openListItem.task, body = value.rawBlocks } :: closeListItems) openListItem :: rest
                }

        BlankLine :: (UnorderedListBlock tight intended closeListItems openListItem) :: rest ->
            let
                value : State
                value =
                    runRawBlockParserInfallible openListItem.body

                tight2 : Bool
                tight2 =
                    if List.member BlankLine value.rawBlocks then
                        False

                    else
                        tight
            in
            succeed
                { linkReferenceDefinitions = state.linkReferenceDefinitions ++ value.linkReferenceDefinitions
                , rawBlocks = UnorderedListBlock tight2 intended ({ task = openListItem.task, body = value.rawBlocks } :: closeListItems) openListItem :: rest
                }

        (OrderedListBlock tight intended marker order closeListItems openListItem) :: rest ->
            let
                value : State
                value =
                    runRawBlockParserInfallible openListItem

                tight2 : Bool
                tight2 =
                    if List.member BlankLine value.rawBlocks then
                        False

                    else
                        tight
            in
            succeed
                { linkReferenceDefinitions = state.linkReferenceDefinitions ++ value.linkReferenceDefinitions
                , rawBlocks = OrderedListBlock tight2 intended marker order (value.rawBlocks :: closeListItems) openListItem :: rest
                }

        BlankLine :: (OrderedListBlock tight intended marker order closeListItems openListItem) :: rest ->
            let
                value : State
                value =
                    runRawBlockParserInfallible openListItem

                tight2 : Bool
                tight2 =
                    if List.member BlankLine value.rawBlocks then
                        False

                    else
                        tight
            in
            succeed
                { linkReferenceDefinitions = state.linkReferenceDefinitions ++ value.linkReferenceDefinitions
                , rawBlocks = OrderedListBlock tight2 intended marker order (value.rawBlocks :: closeListItems) openListItem :: rest
                }

        _ ->
            succeed state



-- Note [Static Parser Structure]
--
-- For performance reasons, it is VERY IMPORTANT that `mergeableBlockAfterOpenBlockOrParagraphParser` and `mergeableBlockNotAfterOpenBlockOrParagraphParser`
-- defined as `var` in javascript (and not as a function taking any, even zero, arguments).
--
-- A `var` is defined once, then re-used for every raw block we parse. If they were functions, the parser
-- structure would need to be re-built for every raw block.
-- Because there are lists involved (in the `oneOf`s), that is expensive.
--
-- All my attempts so far to "DRY" this code below cause a degradation in performance.


mergeableBlockAfterOpenBlockOrParagraphParser : Parser RawBlock
mergeableBlockAfterOpenBlockOrParagraphParser =
    oneOf
        [ parseAsParagraphInsteadOfHtmlBlock
        , blankLine
        , blockQuote
        , Markdown.CodeBlock.parser |> Advanced.backtrackable |> map CodeBlock

        -- NOTE: indented block is not an option immediately after a Body
        , setextLineParser |> Advanced.backtrackable
        , ThematicBreak.parser |> Advanced.backtrackable |> map (\_ -> ThematicBreak)
        , unorderedListBlock True

        -- NOTE: the ordered list block changes its parsing rules when it's right after a Body
        , orderedListBlock True
        , Heading.parser |> Advanced.backtrackable
        , multiLineHtmlParser
        , tableDelimiterInOpenParagraph |> Advanced.backtrackable
        ]


mergeableBlockAfterList : Parser RawBlock
mergeableBlockAfterList =
    oneOf
        [ parseAsParagraphInsteadOfHtmlBlock
        , blankLine
        , blockQuote
        , Markdown.CodeBlock.parser |> Advanced.backtrackable |> map CodeBlock

        -- NOTE: indented block is an option after any non-Body block
        , ThematicBreak.parser |> Advanced.backtrackable |> map (\_ -> ThematicBreak)

        -- NOTE: both the unordered and ordered lists block changes its parsing rules when it's right after a Body
        , unorderedListBlock False
        , orderedListBlock False
        , Heading.parser |> Advanced.backtrackable
        , htmlParser

        -- Note: we know that a table cannot be starting because we define a table as a delimiter row following a header row which gets parsed as a Body initially
        ]


mergeableBlockNotAfterOpenBlockOrParagraphParser : Parser RawBlock
mergeableBlockNotAfterOpenBlockOrParagraphParser =
    oneOf
        [ parseAsParagraphInsteadOfHtmlBlock
        , blankLine
        , blockQuote
        , Markdown.CodeBlock.parser |> Advanced.backtrackable |> map CodeBlock

        -- NOTE: indented block is an option after any non-Body block
        , indentedCodeBlock
        , ThematicBreak.parser |> Advanced.backtrackable |> map (\_ -> ThematicBreak)

        -- NOTE: both the unordered and ordered lists block changes its parsing rules when it's right after a Body
        , unorderedListBlock False
        , orderedListBlock False
        , Heading.parser |> Advanced.backtrackable
        , htmlParser

        -- Note: we know that a table cannot be starting because we define a table as a delimiter row following a header row which gets parsed as a Body initially
        ]


tableDelimiterInOpenParagraph : Parser RawBlock
tableDelimiterInOpenParagraph =
    TableParser.delimiterRowParser
        |> map TableDelimiter


tableRowIfTableStarted : Markdown.Table.Table String -> Parser RawBlock
tableRowIfTableStarted (Markdown.Table.Table headers body) =
    TableParser.bodyRowParser (List.length headers)
        -- We always fill in the whole table so that we don't have an impossible state of a table row without a table header
        |> map (\row -> Table (Markdown.Table.Table headers (body ++ [ row ])))


{-| HTML parsing is intentionally strict in `dillonkearns/elm-markdown`. Paragraphs are supposed to be forgiving.
This function checks to see if something might be an autolink that could be confused with an HTML block because
the line starts with `<`. But it's slightly more lenient, so that things like `<>` that aren't actually parsed as
autolinks are still parsed as paragraphs.

According to CommonMark, valid HTML starts with:

  - `<` + ASCII letter (open tag)
  - `</` + ASCII letter (close tag)
  - `<!` (comment, CDATA, doctype)
  - `<?` (processing instruction)

So if we see `<` followed by anything else (like a digit, underscore, space, etc.),
it's definitely not HTML and should be parsed as paragraph text.

-}
parseAsParagraphInsteadOfHtmlBlock : Parser RawBlock
parseAsParagraphInsteadOfHtmlBlock =
    -- ^<[A-Za-z][A-Za-z0-9.+-]{1,31}:[^<>\x00-\x20]*>
    token (Advanced.Token "<" (Parser.Expecting "<"))
        |. thisIsDefinitelyNotAnHtmlTag
        |. Helpers.chompUntilLineEndOrEnd
        |. Helpers.lineEndOrEnd
        |> Advanced.mapChompedString (\rawLine _ -> rawLine |> UnparsedInlines |> OpenBlockOrParagraph)
        |> Advanced.backtrackable


thisIsDefinitelyNotAnHtmlTag : Parser ()
thisIsDefinitelyNotAnHtmlTag =
    oneOf
        [ -- Space after < means it's not HTML
          token (Advanced.Token " " (Parser.Expecting " "))
        , -- Immediately closing < with > means it's not HTML
          token (Advanced.Token ">" (Parser.Expecting ">"))
        , -- Closing tags (</...) at block level are NOT HTML blocks according to CommonMark.
          -- They should be parsed as paragraph content containing inline raw HTML.
          token (Advanced.Token "/" (Parser.Expecting "/"))
        , -- Character after < that can't start valid HTML (not letter, not /, !, ?)
          -- means it's definitely not HTML. Examples: <33>, <__>, <,>, etc.
          chompIf isNotValidHtmlStartChar (Parser.Expecting "non-HTML start character")
        , -- Autolink pattern: <letter...followed by : @ \ + .>
          chompIf Char.isAlpha (Parser.Expecting "Alpha")
            |. chompWhile (\c -> Char.isAlphaNum c || c == '-')
            |. oneOf
                [ token (Advanced.Token ":" (Parser.Expecting ":"))
                , token (Advanced.Token "@" (Parser.Expecting "@"))
                , token (Advanced.Token "\\" (Parser.Expecting "\\"))
                , token (Advanced.Token "+" (Parser.Expecting "+"))
                , token (Advanced.Token "." (Parser.Expecting "."))
                ]
        ]


{-| Returns True if the character definitely cannot start valid HTML after `<`.
Valid HTML starts are: ASCII letter, `/`, `!`, `?`
-}
isNotValidHtmlStartChar : Char -> Bool
isNotValidHtmlStartChar c =
    not (Char.isAlpha c) && c /= '/' && c /= '!' && c /= '?'


joinStringsPreserveAll : String -> String -> String
joinStringsPreserveAll string1 string2 =
    string1 ++ "\n" ++ string2


joinRawStringsWith : String -> String -> String -> String
joinRawStringsWith joinWith string1 string2 =
    case ( string1, string2 ) of
        ( "", _ ) ->
            string2

        ( _, "" ) ->
            string1

        _ ->
            string1 ++ joinWith ++ string2


exactlyFourSpaces : Parser ()
exactlyFourSpaces =
    oneOf
        [ symbol Token.tab
        , Advanced.backtrackable (symbol Token.space)
            |. oneOf
                [ Advanced.symbol (Advanced.Token "   " (Parser.ExpectingSymbol "Indentation"))
                , Advanced.symbol (Advanced.Token " \t" (Parser.ExpectingSymbol "Indentation"))
                , Advanced.symbol (Advanced.Token "  \t" (Parser.ExpectingSymbol "Indentation"))
                ]
        ]


indentedCodeBlock : Parser RawBlock
indentedCodeBlock =
    succeed IndentedCodeBlock
        |. exactlyFourSpaces
        |= getChompedString Helpers.chompUntilLineEndOrEnd
        |. Helpers.lineEndOrEnd


setextLineParser : Parser RawBlock
setextLineParser =
    let
        setextLevel : a -> Advanced.Token x -> Char -> Advanced.Parser c x a
        setextLevel level levelToken levelChar =
            succeed level
                |. token levelToken
                |. chompWhile ((==) levelChar)
    in
    succeed identity
        |. Whitespace.upToThreeSpaces
        |= oneOf
            [ setextLevel LevelOne Token.equals '='
            , setextLevel LevelTwo Token.minus '-'
            ]
        |. chompWhile Whitespace.isSpaceOrTab
        |. Helpers.lineEndOrEnd
        |> Advanced.mapChompedString
            (\raw level -> SetextLine level raw)
