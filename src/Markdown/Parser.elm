module Markdown.Parser exposing (Renderer, defaultHtmlRenderer, deadEndToString, parse, render)

{-|

@docs Renderer, defaultHtmlRenderer, deadEndToString, parse, render

-}

import Dict
import Helpers
import Html exposing (Html)
import Html.Attributes as Attr
import HtmlParser exposing (Node(..))
import Markdown.Block as Block exposing (Block, Inline, ListItem, Task)
import Markdown.CodeBlock
import Markdown.Html
import Markdown.HtmlRenderer
import Markdown.Inline as Inline
import Markdown.InlineParser
import Markdown.ListItem as ListItem
import Markdown.OrderedList
import Markdown.RawBlock exposing (Attribute, RawBlock(..), UnparsedInlines(..))
import Markdown.UnorderedList
import Parser
import Parser.Advanced as Advanced exposing ((|.), (|=), Nestable(..), Step(..), andThen, chompIf, chompUntil, chompWhile, getChompedString, inContext, int, lazy, loop, map, multiComment, oneOf, problem, succeed, symbol, token)
import Parser.Extra exposing (oneOrMore, zeroOrMore)


{-| A record with functions that define how to render all possible markdown blocks.
These renderers are composed together to give you the final rendered output.

You could render to any type you want. Here are some useful things you might render to:

  - `Html` (using the `defaultHtmlRenderer` provided by this module)
  - Custom `Html`
  - `Element`s from [`mdgriffith/elm-ui`](https://package.elm-lang.org/packages/mdgriffith/elm-ui/latest/)
  - Types from other custom HTML replacement libraries, like [`rtfeldman/elm-css`](https://package.elm-lang.org/packages/rtfeldman/elm-css/latest/) or [`tesk9/accessible-html`](https://package.elm-lang.org/packages/tesk9/accessible-html/latest/)
  - Raw `String`s with [ANSI color codes](http://www.lihaoyi.com/post/BuildyourownCommandLinewithANSIescapecodes.html) for setting rich colors in terminal (CLI) output
  - Plain text with any formatting stripped away (maybe for a String search feature)

-}
type alias Renderer view =
    { heading : { level : Block.HeadingLevel, rawText : String, children : List view } -> view
    , paragraph : List view -> view
    , blockQuote : List view -> view
    , html : Markdown.Html.Renderer (List view -> view)
    , text : String -> view
    , codeSpan : String -> view
    , strong : List view -> view
    , emphasis : List view -> view
    , hardLineBreak : view
    , link : { title : Maybe String, destination : String } -> List view -> Result String view
    , image : { alt : String, src : String, title : Maybe String } -> Result String view
    , unorderedList : List (ListItem view) -> view
    , orderedList : Int -> List (List view) -> view
    , codeBlock : { body : String, language : Maybe String } -> view
    , thematicBreak : view
    }


{-| This renders `Html` in an attempt to be as close as possible to
the HTML output in <https://github.github.com/gfm/>.
-}
defaultHtmlRenderer : Renderer (Html msg)
defaultHtmlRenderer =
    { heading =
        \{ level, children } ->
            case level of
                Block.H1 ->
                    Html.h1 [] children

                Block.H2 ->
                    Html.h2 [] children

                Block.H3 ->
                    Html.h3 [] children

                Block.H4 ->
                    Html.h4 [] children

                Block.H5 ->
                    Html.h5 [] children

                Block.H6 ->
                    Html.h6 [] children
    , paragraph = Html.p []
    , hardLineBreak = Html.br [] []
    , blockQuote = Html.blockquote []
    , strong =
        \children -> Html.strong [] children
    , emphasis =
        \children -> Html.em [] children
    , codeSpan =
        \content -> Html.code [] [ Html.text content ]
    , link =
        \link content ->
            case link.title of
                Just title ->
                    Html.a
                        [ Attr.href link.destination
                        , Attr.title title
                        ]
                        content
                        |> Ok

                Nothing ->
                    Html.a [ Attr.href link.destination ] content
                        |> Ok
    , image =
        \imageInfo ->
            case imageInfo.title of
                Just title ->
                    Html.img
                        [ Attr.src imageInfo.src
                        , Attr.alt imageInfo.alt
                        , Attr.title title
                        ]
                        []
                        |> Ok

                Nothing ->
                    Html.img
                        [ Attr.src imageInfo.src
                        , Attr.alt imageInfo.alt
                        ]
                        []
                        |> Ok
    , text =
        Html.text
    , unorderedList =
        \items ->
            Html.ul []
                (items
                    |> List.map
                        (\item ->
                            case item of
                                Block.ListItem task children ->
                                    let
                                        checkbox =
                                            case task of
                                                Block.NoTask ->
                                                    Html.text ""

                                                Block.IncompleteTask ->
                                                    Html.input
                                                        [ Attr.disabled True
                                                        , Attr.checked False
                                                        , Attr.type_ "checkbox"
                                                        ]
                                                        []

                                                Block.CompletedTask ->
                                                    Html.input
                                                        [ Attr.disabled True
                                                        , Attr.checked True
                                                        , Attr.type_ "checkbox"
                                                        ]
                                                        []
                                    in
                                    Html.li [] (checkbox :: children)
                        )
                )
    , orderedList =
        \startingIndex items ->
            Html.ol
                (case startingIndex of
                    1 ->
                        [ Attr.start startingIndex ]

                    _ ->
                        []
                )
                (items
                    |> List.map
                        (\itemBlocks ->
                            Html.li []
                                itemBlocks
                        )
                )
    , html = Markdown.Html.oneOf []
    , codeBlock =
        \{ body, language } ->
            Html.pre []
                [ Html.code []
                    [ Html.text body
                    ]
                ]
    , thematicBreak = Html.hr [] []
    }


renderStyled : Renderer view -> List Inline -> Result String (List view)
renderStyled renderer styledStrings =
    styledStrings
        |> List.foldr (foldThing renderer) []
        |> combineResults


foldThing : Renderer view -> Inline -> List (Result String view) -> List (Result String view)
foldThing renderer topLevelInline soFar =
    --                    Ok styledLine ->
    --                        (renderStyled renderer styledLine
    --                            |> Result.andThen
    --                                (\children ->
    --                                    renderer.link { title = link.title, destination = destination } children
    --                                )
    --                        )
    --                            :: soFar
    --
    --                    Err error ->
    --                        (error |> List.map deadEndToString |> List.map Err)
    --                            ++ soFar
    --Block.InlineContent inline ->
    case renderSingleInline renderer topLevelInline of
        Just inline ->
            inline :: soFar

        Nothing ->
            soFar


renderSingleInline : Renderer view -> Block.Inline -> Maybe (Result String view)
renderSingleInline renderer inline =
    case inline of
        Block.Strong innerInlines ->
            innerInlines
                |> renderStyled renderer
                |> Result.map renderer.strong
                |> Just

        Block.Emphasis innerInlines ->
            innerInlines
                |> renderStyled renderer
                |> Result.map renderer.emphasis
                |> Just

        Block.Image src title children ->
            renderer.image { alt = Block.extractText children, src = src, title = title }
                |> Just

        Block.Text string ->
            renderer.text string
                |> Ok
                |> Just

        Block.CodeSpan string ->
            renderer.codeSpan string
                |> Ok
                |> Just

        Block.Link destination title inlines ->
            renderStyled renderer inlines
                |> Result.andThen
                    (\children ->
                        renderer.link { title = title, destination = destination } children
                    )
                |> Just

        Block.HardLineBreak ->
            renderer.hardLineBreak
                |> Ok
                |> Just

        Block.HtmlInline html ->
            case html of
                Block.HtmlElement tag attributes children ->
                    renderHtmlNode renderer tag attributes children
                        |> Just

                _ ->
                    Nothing



--renderHtmlNode : Renderer view -> String -> List Attribute -> List Block -> Result String view
--renderHtmlNode renderer tag attributes children =
--    renderHtml tag
--        attributes
--        children
--        renderer.html
--        (renderHelper renderer children)
--, html : Markdown.Html.Renderer (List view -> view)
--    renderer.html
--:: soFar


renderHelper :
    Renderer view
    -> List Block
    -> List (Result String view)
renderHelper renderer blocks =
    List.filterMap
        (\block ->
            case block of
                Block.Heading level content ->
                    renderStyled renderer content
                        |> Result.map
                            (\children ->
                                renderer.heading
                                    { level = level
                                    , rawText = Block.extractText content
                                    , children = children
                                    }
                            )
                        |> Just

                Block.Paragraph content ->
                    renderStyled renderer content
                        |> Result.map renderer.paragraph
                        |> Just

                Block.HtmlBlock html ->
                    case html of
                        Block.HtmlElement tag attributes children ->
                            renderHtmlNode renderer tag attributes children
                                |> Just

                        _ ->
                            Nothing

                Block.UnorderedListBlock items ->
                    items
                        |> List.map
                            (\(Block.ListItem task children) ->
                                children
                                    |> renderStyled renderer
                                    |> Result.map (\renderedBody -> Block.ListItem task renderedBody)
                            )
                        |> combineResults
                        |> Result.map renderer.unorderedList
                        |> Just

                Block.OrderedListBlock startingIndex items ->
                    items
                        |> List.map (renderStyled renderer)
                        |> combineResults
                        |> Result.map (renderer.orderedList startingIndex)
                        |> Just

                Block.CodeBlock codeBlock ->
                    codeBlock
                        |> renderer.codeBlock
                        |> Ok
                        |> Just

                Block.ThematicBreak ->
                    Ok renderer.thematicBreak
                        |> Just

                Block.BlockQuote nestedBlocks ->
                    renderHelper renderer nestedBlocks
                        |> combineResults
                        |> Result.map renderer.blockQuote
                        |> Just
        )
        blocks


{-| Apply a `Markdown.Parser.Renderer` to turn parsed `Block`s into your rendered
markdown view.
-}
render :
    Renderer view
    -> List Block
    -> Result String (List view)
render renderer ast =
    ast
        |> renderHelper renderer
        |> combineResults


renderHtml :
    String
    -> List Attribute
    -> List Block
    -> Markdown.Html.Renderer (List view -> view)
    -> List (Result String view)
    -> Result String view
renderHtml tagName attributes children (Markdown.HtmlRenderer.HtmlRenderer htmlRenderer) renderedChildren =
    renderedChildren
        |> combineResults
        |> Result.andThen
            (\okChildren ->
                htmlRenderer tagName attributes children
                    |> Result.map
                        (\myRenderer -> myRenderer okChildren)
            )


combineResults : List (Result x a) -> Result x (List a)
combineResults =
    List.foldr (Result.map2 (::)) (Ok [])


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


renderHtmlNode : Renderer view -> String -> List Attribute -> List Block -> Result String view
renderHtmlNode renderer tag attributes children =
    renderHtml tag
        attributes
        children
        renderer.html
        (renderHelper renderer children)


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

        Inline.HtmlInline string attributes htmlValue ->
            let
                inlines =
                    case Advanced.run multiParser2 htmlValue of
                        Ok children ->
                            children

                        Err error ->
                            -- TODO pass up parsing error
                            []
            in
            Block.HtmlInline (Block.HtmlElement string attributes inlines)

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
            --Markdown.InlineParser.parse Dict.empty unparsedInlines
            unparsedInlines
                |> inlineParseHelper
                |> (\styledLine -> just (Block.Paragraph styledLine))

        Html tagName attributes children ->
            Block.HtmlBlock (Block.HtmlElement tagName attributes children)
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
                |> map Block.UnorderedListBlock
                |> map Just

        OrderedListBlock startingIndex unparsedInlines ->
            unparsedInlines
                |> List.map (parseRawInline identity)
                |> combine
                |> map (Block.OrderedListBlock startingIndex)
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

        HtmlComment string ->
            Block.HtmlBlock (Block.HtmlComment string)
                |> just


just value =
    succeed (Just value)


parseRawInline : (List Inline -> a) -> UnparsedInlines -> Advanced.Parser c Parser.Problem a
parseRawInline wrap unparsedInlines =
    --Markdown.InlineParser.parse Dict.empty unparsedInlines
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
                    -- TODO is this right?
                    Body
                        (UnparsedInlines innerText)
                        |> Advanced.succeed

                HtmlParser.Element tag attributes children ->
                    Advanced.andThen
                        (\parsedChildren ->
                            Advanced.succeed
                                (Html tag
                                    attributes
                                    parsedChildren
                                )
                        )
                        (nodesToBlocksParser children)

                Comment string ->
                    succeed <| HtmlComment string
        )
        parser


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
        , htmlParser |> keepLooping
        , plainLine |> keepLooping
        , succeed (Done revStmts)
        ]


joinStringsPreserveAll string1 string2 =
    let
        string1Trimmed =
            --String.trimRight
            string1

        string2Trimmed =
            --String.trimRight
            string2
    in
    String.concat
        [ string1Trimmed
        , "\n"
        , string2Trimmed
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
    Advanced.run
        multiParser2
        (input
         --|> Debug.log "input"
        )
