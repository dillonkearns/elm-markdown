module Markdown.Parser exposing (parse, deadEndToString)

{-|

@docs parse, deadEndToString

-}

import Dict exposing (Dict)
import Helpers
import HtmlParser exposing (Node(..))
import Markdown.Block as Block exposing (Block, Inline, ListItem, Task)
import Markdown.CodeBlock
import Markdown.Heading as Heading
import Markdown.Helpers exposing (isEven)
import Markdown.Inline as Inline
import Markdown.InlineParser
import Markdown.LinkReferenceDefinition as LinkReferenceDefinition exposing (LinkReferenceDefinition)
import Markdown.ListItem as ListItem
import Markdown.OrderedList
import Markdown.RawBlock as RawBlock exposing (Attribute, RawBlock(..), SetextLevel(..), UnparsedInlines(..))
import Markdown.Table
import Markdown.TableParser as TableParser
import Markdown.UnorderedList
import Parser
import Parser.Advanced as Advanced exposing ((|.), (|=), Nestable(..), Step(..), andThen, chompIf, chompWhile, getChompedString, getIndent, loop, map, oneOf, problem, succeed, symbol, token)
import Parser.Token as Token
import String exposing (repeat, trim)
import ThematicBreak
import Whitespace


{-| Try parsing a markdown String into `Markdown.Block.Block`s.

Often you'll want to render these `Block`s directly:

    render renderer markdown =
        markdown
            |> Markdown.Parser.parse
            |> Result.mapError deadEndsToString
            |> Result.andThen (\ast -> Markdown.Renderer.render renderer ast)

    deadEndsToString deadEnds =
        deadEnds
            |> List.map Markdown.Parser.deadEndToString
            |> String.join "\n"

But you can also do a lot with the `Block`s before passing them through:

  - Transform the `Block`s ([example: make each heading one level deeper](TODO))
  - Use the blocks to gather metadata about the markdown document ([example: building a table of contents from `Block`s](TODO))

-}
parse : String -> Result (List (Advanced.DeadEnd String Parser.Problem)) (List Block)
parse input =
    -- first parse the file as raw blocks
    case Advanced.run (rawBlockParser |. Helpers.endOfFile) input of
        Err e ->
            Err e

        Ok v ->
            -- then parse the inlines of each raw block
            case parseAllInlines v of
                Err e ->
                    -- NOTE these messages get an incorrect location,
                    -- because they are parsed outside of the main (raw block) parser context.
                    Advanced.run (Advanced.problem e) ""

                Ok blocks ->
                    let
                        -- TODO find a better way to do this
                        -- e.g. make sure they are never generated
                        isNotEmptyParagraph block =
                            case block of
                                Block.Paragraph [] ->
                                    False

                                _ ->
                                    True
                    in
                    Ok (List.filter isNotEmptyParagraph blocks)


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
        mappedReferencesDict =
            referencesDict
                |> List.map (Tuple.mapSecond (\{ destination, title } -> ( destination, title )))
                |> Dict.fromList
    in
    Markdown.InlineParser.parse mappedReferencesDict unparsedInlines
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
                    if level |> isEven then
                        Block.Strong [ Inline.Emphasis (level - 2) inlines |> mapInline ]

                    else
                        Block.Emphasis [ Inline.Emphasis (level - 1) inlines |> mapInline ]

        Inline.Strikethrough inlines ->
            Block.Strikethrough (inlines |> List.map mapInline)


toHeading : Int -> Result Parser.Problem Block.HeadingLevel
toHeading level =
    case level of
        1 ->
            Ok Block.H1

        2 ->
            Ok Block.H2

        3 ->
            Ok Block.H3

        4 ->
            Ok Block.H4

        5 ->
            Ok Block.H5

        6 ->
            Ok Block.H6

        _ ->
            Err ("A heading with 1 to 6 #'s, but found " ++ String.fromInt level |> Parser.Expecting)


type InlineResult
    = EmptyBlock
    | ParsedBlock Block
    | InlineProblem Parser.Problem


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
            case toHeading level of
                Ok parsedLevel ->
                    unparsedInlines
                        |> inlineParseHelper linkReferences
                        |> Block.Heading parsedLevel
                        |> ParsedBlock

                Err e ->
                    InlineProblem e

        OpenBlockOrParagraph unparsedInlines ->
            unparsedInlines
                |> inlineParseHelper linkReferences
                |> Block.Paragraph
                |> ParsedBlock

        Html html ->
            Block.HtmlBlock html
                |> ParsedBlock

        UnorderedListBlock tight intended unparsedItems _ ->
            let
                parseItem rawBlockTask rawBlocks =
                    let
                        blocks =
                            case parseAllInlines { linkReferenceDefinitions = linkReferences, rawBlocks = rawBlocks } of
                                Ok parsedBlocks ->
                                    parsedBlocks

                                --TODO: pass this Err e
                                Err e ->
                                    []

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
                    case parseAllInlines { linkReferenceDefinitions = linkReferences, rawBlocks = rawBlocks } of
                        Ok parsedBlocks ->
                            parsedBlocks

                        --TODO: pass this Err e
                        Err e ->
                            []
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

        RawBlock.BlockQuote rawBlocks ->
            EmptyBlock

        ParsedBlockQuote rawBlocks ->
            case parseAllInlines { linkReferenceDefinitions = linkReferences, rawBlocks = rawBlocks } of
                Ok parsedBlocks ->
                    Block.BlockQuote parsedBlocks
                        |> ParsedBlock

                Err e ->
                    InlineProblem e

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
        parseListItem listmarker intended unparsedListItem =
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
                    (parseListItem listmarker intended unparsedListItem)
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
    HtmlParser.html
        |> Advanced.andThen xmlNodeToHtmlNode


xmlNodeToHtmlNode : Node -> Parser RawBlock
xmlNodeToHtmlNode xmlNode =
    case xmlNode of
        HtmlParser.Text innerText ->
            OpenBlockOrParagraph (UnparsedInlines innerText)
                |> succeed

        HtmlParser.Element tag attributes children ->
            case nodesToBlocks children of
                Ok parsedChildren ->
                    Block.HtmlElement tag attributes parsedChildren
                        |> RawBlock.Html
                        |> succeed

                Err err ->
                    problem err

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


textNodeToBlocks : String -> List Block
textNodeToBlocks textNodeValue =
    parse textNodeValue
        |> Result.withDefault []


nodeToRawBlock : Node -> Block.Html Block
nodeToRawBlock node =
    case node of
        HtmlParser.Text innerText ->
            Block.HtmlComment "TODO this never happens, but use types to drop this case."

        HtmlParser.Element tag attributes children ->
            let
                parseChild child =
                    case child of
                        HtmlParser.Text text ->
                            textNodeToBlocks text

                        _ ->
                            [ nodeToRawBlock child |> Block.HtmlBlock ]
            in
            Block.HtmlElement tag
                attributes
                (List.concatMap parseChild children)

        Comment string ->
            Block.HtmlComment string

        Cdata string ->
            Block.Cdata string

        ProcessingInstruction string ->
            Block.ProcessingInstruction string

        Declaration declarationType content ->
            Block.HtmlDeclaration declarationType content


nodesToBlocks : List Node -> Result Parser.Problem (List Block)
nodesToBlocks children =
    nodesToBlocksHelp children []


nodesToBlocksHelp : List Node -> List Block -> Result Parser.Problem (List Block)
nodesToBlocksHelp remaining soFar =
    case remaining of
        node :: rest ->
            case childToBlocks node soFar of
                Ok newSoFar ->
                    nodesToBlocksHelp rest newSoFar

                Err e ->
                    Err e

        [] ->
            Ok (List.reverse soFar)


{-| Add the blocks from this node to the passed-in list of blocks
-}
childToBlocks : Node -> List Block -> Result Parser.Problem (List Block)
childToBlocks node blocks =
    case node of
        Element tag attributes children ->
            case nodesToBlocks children of
                Ok childrenAsBlocks ->
                    let
                        block =
                            Block.HtmlElement tag attributes childrenAsBlocks
                                |> Block.HtmlBlock
                    in
                    Ok (block :: blocks)

                Err err ->
                    Err err

        Text innerText ->
            case parse innerText of
                Ok value ->
                    Ok (List.reverse value ++ blocks)

                Err error ->
                    Err
                        (Parser.Expecting
                            (error
                                |> List.map deadEndToString
                                |> String.join "\n"
                            )
                        )

        Comment string ->
            Ok (Block.HtmlBlock (Block.HtmlComment string) :: blocks)

        Cdata string ->
            Ok (Block.HtmlBlock (Block.Cdata string) :: blocks)

        ProcessingInstruction string ->
            Ok (Block.HtmlBlock (Block.ProcessingInstruction string) :: blocks)

        Declaration declarationType content ->
            Ok (Block.HtmlBlock (Block.HtmlDeclaration declarationType content) :: blocks)


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


rawBlockParser : Parser State
rawBlockParser =
    loop
        { linkReferenceDefinitions = []
        , rawBlocks = []
        }
        stepRawBlock
        |> andThen completeBlocks


parseAllInlines : State -> Result Parser.Problem (List Block)
parseAllInlines state =
    parseAllInlinesHelp state state.rawBlocks []


parseAllInlinesHelp : State -> List RawBlock -> List Block -> Result Parser.Problem (List Block)
parseAllInlinesHelp state rawBlocks parsedBlocks =
    case rawBlocks of
        rawBlock :: rest ->
            case parseInlines state.linkReferenceDefinitions rawBlock of
                ParsedBlock newParsedBlock ->
                    parseAllInlinesHelp state rest (newParsedBlock :: parsedBlocks)

                EmptyBlock ->
                    -- ignore empty blocks
                    parseAllInlinesHelp state rest parsedBlocks

                InlineProblem e ->
                    Err e

        [] ->
            Ok parsedBlocks


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
                    succeed
                        { linkReferenceDefinitions = state.linkReferenceDefinitions
                        , rawBlocks =
                            BlockQuote (joinRawStringsWith "\n" body2 body1)
                                :: rest
                        }

                _ ->
                    case Advanced.run rawBlockParser body2 of
                        Ok value ->
                            succeed
                                { linkReferenceDefinitions = state.linkReferenceDefinitions ++ value.linkReferenceDefinitions
                                , rawBlocks = newRawBlock :: (value.rawBlocks |> ParsedBlockQuote) :: rest
                                }

                        Err e ->
                            problem (Parser.Problem (deadEndsToString e))

        ( _, (UnorderedListBlock tight intended1 closeListItems2 openListItem2) :: rest ) ->
            case newRawBlock of
                UnorderedListBlock _ intended2 closeListItems1 openListItem1 ->
                    if openListItem2.marker == openListItem1.marker then
                        case Advanced.run rawBlockParser openListItem2.body of
                            Ok value ->
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

                            Err e ->
                                problem (Parser.Problem (deadEndsToString e))

                    else
                        case Advanced.run rawBlockParser openListItem2.body of
                            Ok value ->
                                let
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

                            Err e ->
                                problem (Parser.Problem (deadEndsToString e))

                OpenBlockOrParagraph (UnparsedInlines body1) ->
                    succeed
                        { linkReferenceDefinitions = state.linkReferenceDefinitions
                        , rawBlocks =
                            UnorderedListBlock tight intended1 closeListItems2 { openListItem2 | body = joinRawStringsWith "\n" openListItem2.body body1 }
                                :: rest
                        }

                _ ->
                    case Advanced.run rawBlockParser openListItem2.body of
                        Ok value ->
                            let
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

                        Err e ->
                            problem (Parser.Problem (deadEndsToString e))

        -- OrderedListBlock Bool Int OrderedListMarker Int (List (List RawBlock)) String
        -- (\item -> OrderedListBlock True item.intended item.marker item.order [] item.body)
        ( _, (OrderedListBlock tight intended1 marker order closeListItems2 openListItem2) :: rest ) ->
            case newRawBlock of
                OrderedListBlock _ intended2 marker2 _ closeListItems1 openListItem1 ->
                    if marker == marker2 then
                        case Advanced.run rawBlockParser openListItem2 of
                            Ok value ->
                                let
                                    tight2 =
                                        if List.member BlankLine value.rawBlocks then
                                            False

                                        else
                                            tight
                                in
                                succeed
                                    { linkReferenceDefinitions = state.linkReferenceDefinitions ++ value.linkReferenceDefinitions
                                    , rawBlocks = OrderedListBlock tight2 intended2 marker order (value.rawBlocks :: closeListItems2) openListItem1 :: rest
                                    }

                            Err e ->
                                problem (Parser.Problem (deadEndsToString e))

                    else
                        case Advanced.run rawBlockParser openListItem2 of
                            Ok value ->
                                let
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

                            Err e ->
                                problem (Parser.Problem (deadEndsToString e))

                OpenBlockOrParagraph (UnparsedInlines body1) ->
                    succeed
                        { linkReferenceDefinitions = state.linkReferenceDefinitions
                        , rawBlocks =
                            OrderedListBlock tight intended1 marker order closeListItems2 (openListItem2 ++ "\n" ++ body1)
                                :: rest
                        }

                _ ->
                    case Advanced.run rawBlockParser openListItem2 of
                        Ok value ->
                            let
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

                        Err e ->
                            problem (Parser.Problem (deadEndsToString e))

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
            case Advanced.run rawBlockParser openListItem2 of
                Ok value ->
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

                Err e ->
                    problem (Parser.Problem (deadEndsToString e))

        ( _, BlankLine :: (UnorderedListBlock tight intended1 closeListItems2 openListItem2) :: rest ) ->
            case Advanced.run rawBlockParser openListItem2.body of
                Ok value ->
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

                Err e ->
                    problem (Parser.Problem (deadEndsToString e))

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
                    completeOrMergeUnorderedListBlock state newString =
                        { state
                            | rawBlocks =
                                ({ openListItem | body = joinRawStringsWith "\n" openListItem.body newString }
                                    |> UnorderedListBlock tight intended closeListItems
                                )
                                    :: rest
                        }

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
                    completeOrMergeUnorderedListBlock state newString =
                        { state
                            | rawBlocks =
                                ({ openListItem | body = joinRawStringsWith "\n" openListItem.body newString }
                                    |> UnorderedListBlock tight intended closeListItems
                                )
                                    :: rest
                        }

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
                    completeOrMergeUnorderedListBlock state newString =
                        { state
                            | rawBlocks =
                                OrderedListBlock tight intended marker order closeListItems (openListItem ++ "\n" ++ newString)
                                    :: rest
                        }

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
                    completeOrMergeUnorderedListBlock state newString =
                        { state
                            | rawBlocks =
                                OrderedListBlock tight intended marker order closeListItems (openListItem ++ "\n" ++ newString)
                                    :: rest
                        }

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
    -> Parser State --Result Parser.Problem (List Block)
completeBlocks state =
    case state.rawBlocks of
        (BlockQuote body2) :: rest ->
            case Advanced.run rawBlockParser body2 of
                Ok value ->
                    succeed
                        { linkReferenceDefinitions = state.linkReferenceDefinitions ++ value.linkReferenceDefinitions
                        , rawBlocks = (value.rawBlocks |> ParsedBlockQuote) :: rest
                        }

                Err error ->
                    problem (Parser.Problem (deadEndsToString error))

        (UnorderedListBlock tight intended closeListItems openListItem) :: rest ->
            case Advanced.run rawBlockParser openListItem.body of
                Ok value ->
                    let
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

                Err e ->
                    problem (Parser.Problem (deadEndsToString e))

        BlankLine :: (UnorderedListBlock tight intended closeListItems openListItem) :: rest ->
            case Advanced.run rawBlockParser openListItem.body of
                Ok value ->
                    let
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

                Err e ->
                    problem (Parser.Problem (deadEndsToString e))

        (OrderedListBlock tight intended marker order closeListItems openListItem) :: rest ->
            case Advanced.run rawBlockParser openListItem of
                Ok value ->
                    let
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

                Err e ->
                    problem (Parser.Problem (deadEndsToString e))

        BlankLine :: (OrderedListBlock tight intended marker order closeListItems openListItem) :: rest ->
            case Advanced.run rawBlockParser openListItem of
                Ok value ->
                    let
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

                Err e ->
                    problem (Parser.Problem (deadEndsToString e))

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
        , htmlParser
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
