module BlockTransformTests exposing (suite)

import Expect
import Markdown.Block as Block exposing (..)
import Test exposing (..)


suite : Test
suite =
    only <|
        describe "transform blocks"
            [ test "map links" <|
                \() ->
                    let
                        httpLinksToHttps : String -> String
                        httpLinksToHttps =
                            String.replace "http://" "https://"
                    in
                    [ Paragraph
                        [ Link "http://elm-lang.org" Nothing [ Text "elm-lang search results" ]
                        ]
                    ]
                        |> mapInlines
                            (\inline ->
                                case inline of
                                    Link destination title inlines ->
                                        Link (httpLinksToHttps destination) title inlines

                                    _ ->
                                        inline
                            )
                        |> Expect.equal
                            [ Paragraph
                                [ Link "https://elm-lang.org" Nothing [ Text "elm-lang search results" ]
                                ]
                            ]
            ]


map : (Block -> value) -> List Block -> List value
map mapFn blocks =
    blocks
        |> List.map mapFn


mapInlines : (Inline -> Inline) -> List Block -> List Block
mapInlines mapFn blocks =
    blocks
        |> List.map (walkInlines mapFn)


walkInlines : (Inline -> Inline) -> Block -> Block
walkInlines function block =
    walk (walkInlinesHelp function) block


walkInlinesHelp : (Inline -> Inline) -> Block -> Block
walkInlinesHelp function block =
    case block of
        Block.Paragraph inlines ->
            List.map (inlineParserWalk function) inlines
                |> Paragraph

        --Heading rawText level inlines ->
        --    List.map (Inline.walk function) inlines
        --        |> Heading rawText level
        --
        --PlainInlines inlines ->
        --    List.map (Inline.walk function) inlines
        --        |> PlainInlines
        --
        _ ->
            block


inlineParserWalk : (Inline -> Inline) -> Inline -> Inline
inlineParserWalk function inline =
    case inline of
        Link url maybeTitle inlines ->
            List.map (inlineParserWalk function) inlines
                |> Link url maybeTitle
                |> function

        Image url maybeTitle inlines ->
            List.map (inlineParserWalk function) inlines
                |> Image url maybeTitle
                |> function

        Emphasis inlines ->
            List.map (inlineParserWalk function) inlines
                |> Emphasis
                |> function

        --HtmlInline tag attrs inlines ->
        --    List.map (inlineParserWalk function) inlines
        --        |> HtmlInline tag attrs
        --        |> function
        _ ->
            function inline



--walk : (Inline -> Inline) -> Inline -> Inline
--walk function inline =
--    case inline of
--        Link url maybeTitle inlines ->
--            List.map (walk function) inlines
--                |> Link url maybeTitle
--                |> function
--
--        Image url maybeTitle inlines ->
--            List.map (walk function) inlines
--                |> Image url maybeTitle
--                |> function
--
--        HtmlInline html ->
--            --List.map (walk function) inlines
--            --    |> HtmlInline tag attrs
--            function inline
--
--        Emphasis inlines ->
--            List.map (walk function) inlines
--                |> Emphasis
--                |> function
--
--        _ ->
--            function inline


walk : (Block -> Block) -> Block -> Block
walk function block =
    case block of
        BlockQuote blocks ->
            List.map (walk function) blocks
                |> BlockQuote
                |> function

        --Block.OrderedList startingIndex items ->
        --    List.map (List.map (walk function)) items
        --        |> Block.OrderedList startingIndex
        --        |> function
        _ ->
            function block


validateMap : (Block -> Result error value) -> List Block -> Result error (List value)
validateMap mapFn blocks =
    blocks
        |> List.map mapFn
        |> combine


{-| Combine a list of results into a single result (holding a list).
-}
combine : List (Result x a) -> Result x (List a)
combine =
    List.foldr (Result.map2 (::)) (Ok [])
