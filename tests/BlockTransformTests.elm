module BlockTransformTests exposing (suite)

import Dict
import Expect
import Markdown.Block as Block exposing (..)
import Test exposing (..)


resolveLinkInInline : Inline -> Result String Inline
resolveLinkInInline inline =
    case inline of
        Link destination title inlines ->
            destination
                |> lookupLink
                |> Result.map (\resolvedLink -> Link resolvedLink title inlines)

        _ ->
            Ok inline


lookupLink : String -> Result String String
lookupLink key =
    case key of
        "elm-lang" ->
            Ok "https://elm-lang.org"

        _ ->
            Err <| "Couldn't find key " ++ key


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
                        [ Link "http://elm-lang.org" Nothing [ Text "elm-lang homepage" ]
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
                                [ Link "https://elm-lang.org" Nothing [ Text "elm-lang homepage" ]
                                ]
                            ]
            , test "validate links - valid" <|
                \() ->
                    [ Paragraph
                        [ Link "elm-lang" Nothing [ Text "elm-lang homepage" ]
                        ]
                    ]
                        |> validateMapInlines resolveLinkInInline
                        |> Expect.equal
                            (Ok
                                [ Paragraph
                                    [ Link "https://elm-lang.org" Nothing [ Text "elm-lang homepage" ]
                                    ]
                                ]
                            )
            , test "validate links - invalid" <|
                \() ->
                    [ Paragraph
                        [ Link "angular" Nothing [ Text "elm-lang homepage" ]
                        ]
                    ]
                        |> validateMapInlines resolveLinkInInline
                        |> Expect.equal (Err [ "Couldn't find key angular" ])
            , test "add slugs" <|
                \() ->
                    let
                        gatherHeadingOccurences : List Block -> ( Dict.Dict String Int, List (BlockWithMeta (Maybe String)) )
                        gatherHeadingOccurences =
                            mapAccuml
                                (\soFar block ->
                                    case block of
                                        Heading level inlines ->
                                            let
                                                inlineText : String
                                                inlineText =
                                                    Block.extractInlineText inlines

                                                occurenceModifier : String
                                                occurenceModifier =
                                                    soFar
                                                        |> Dict.get inlineText
                                                        |> Maybe.map String.fromInt
                                                        |> Maybe.withDefault ""
                                            in
                                            ( soFar |> trackOccurence inlineText
                                            , BlockWithMeta (Heading level inlines) (Just (inlineText ++ occurenceModifier))
                                            )

                                        _ ->
                                            ( soFar
                                            , BlockWithMeta block Nothing
                                            )
                                )
                                Dict.empty

                        trackOccurence : String -> Dict.Dict String Int -> Dict.Dict String Int
                        trackOccurence value occurences =
                            occurences
                                |> Dict.update value
                                    (\maybeOccurence ->
                                        case maybeOccurence of
                                            Just count ->
                                                Just <| count + 1

                                            Nothing ->
                                                Just 1
                                    )
                    in
                    [ Heading H1 [ Text "foo" ]
                    , Heading H1 [ Text "bar" ]
                    , Heading H1 [ Text "foo" ]
                    ]
                        |> gatherHeadingOccurences
                        |> Expect.equal
                            ( Dict.fromList
                                [ ( "bar", 1 )
                                , ( "foo", 2 )
                                ]
                            , [ BlockWithMeta (Heading H1 [ Text "foo" ]) (Just "foo")
                              , BlockWithMeta (Heading H1 [ Text "bar" ]) (Just "bar")
                              , BlockWithMeta (Heading H1 [ Text "foo" ]) (Just "foo1")
                              ]
                            )
            ]


mapAccuml : (soFar -> Block -> ( soFar, mappedValue )) -> soFar -> List Block -> ( soFar, List mappedValue )
mapAccuml function initialValue blocks =
    let
        ( accFinal, generatedList ) =
            List.foldl
                (\x ( acc1, ys ) ->
                    let
                        ( acc2, y ) =
                            function acc1 x
                    in
                    ( acc2, y :: ys )
                )
                ( initialValue, [] )
                blocks
    in
    ( accFinal, List.reverse generatedList )


type BlockWithMeta meta
    = BlockWithMeta Block meta


validateMapInlines : (Inline -> Result error Inline) -> List Block -> Result (List error) (List Block)
validateMapInlines function blocks =
    let
        newThing : Block -> Result (List error) Block
        newThing block =
            case block of
                Paragraph inlines ->
                    inlines
                        |> List.map (inlineParserValidateWalk function)
                        |> combine
                        |> Result.map Paragraph

                _ ->
                    Debug.todo ""
    in
    blocks
        |> validateMap newThing


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


inlineParserValidateWalk : (Inline -> Result error Inline) -> Inline -> Result (List error) Inline
inlineParserValidateWalk function inline =
    case inline of
        Link url maybeTitle inlines ->
            List.map (inlineParserValidateWalk function) inlines
                |> combine
                |> Result.andThen
                    (\nestedInlines ->
                        Link url maybeTitle nestedInlines
                            |> function
                            |> Result.mapError List.singleton
                    )

        --Image url maybeTitle inlines ->
        --    List.map (inlineParserWalk function) inlines
        --        |> Image url maybeTitle
        --        |> function
        --
        --Emphasis inlines ->
        --    List.map (inlineParserWalk function) inlines
        --        |> Emphasis
        --        |> function
        --HtmlInline tag attrs inlines ->
        --    List.map (inlineParserWalk function) inlines
        --        |> HtmlInline tag attrs
        --        |> function
        _ ->
            function inline
                |> Result.mapError List.singleton


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
