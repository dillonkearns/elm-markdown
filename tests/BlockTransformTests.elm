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
                        |> Block.mapInlines
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
                        |> Block.validateMapInlines resolveLinkInInline
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
                        |> Block.validateMapInlines resolveLinkInInline
                        |> Expect.equal (Err [ "Couldn't find key angular" ])
            , test "maximum heading level" <|
                \() ->
                    let
                        maximumHeadingLevel : List Block -> Maybe HeadingLevel
                        maximumHeadingLevel blocks =
                            blocks
                                |> Block.foldl
                                    (\block maxSoFar ->
                                        case block of
                                            Heading level _ ->
                                                if Block.headingLevelToInt level > (maxSoFar |> Maybe.map Block.headingLevelToInt |> Maybe.withDefault 0) then
                                                    Just level

                                                else
                                                    maxSoFar

                                            _ ->
                                                maxSoFar
                                    )
                                    Nothing
                    in
                    [ Heading H1 [ Text "Document" ]
                    , Heading H2 [ Text "Section A" ]
                    , Heading H3 [ Text "Subsection" ]
                    , Heading H2 [ Text "Section B" ]
                    ]
                        |> maximumHeadingLevel
                        |> Expect.equal (Just H3)
            , test "maximum heading level with container block" <|
                \() ->
                    let
                        maximumHeadingLevel : List Block -> Maybe HeadingLevel
                        maximumHeadingLevel blocks =
                            blocks
                                |> Block.foldl
                                    (\block maxSoFar ->
                                        case block of
                                            Heading level _ ->
                                                if Block.headingLevelToInt level > (maxSoFar |> Maybe.map Block.headingLevelToInt |> Maybe.withDefault 0) then
                                                    Just level

                                                else
                                                    maxSoFar

                                            _ ->
                                                maxSoFar
                                    )
                                    Nothing
                    in
                    [ Heading H1 [ Text "Document" ]
                    , Heading H2 [ Text "Section A" ]
                    , Heading H3 [ Text "Subsection" ]
                    , BlockQuote
                        [ Heading H4 [ Text "Heading in container block" ] ]
                    , Heading H2 [ Text "Section B" ]
                    ]
                        |> maximumHeadingLevel
                        |> Expect.equal (Just H4)
            , test "traversal order is depth-first" <|
                \() ->
                    let
                        gatherHeadingText : List Block -> List String
                        gatherHeadingText blocks =
                            blocks
                                |> Block.foldl
                                    (\block maxSoFar ->
                                        case block of
                                            Heading _ [ Text text ] ->
                                                text :: maxSoFar

                                            _ ->
                                                maxSoFar
                                    )
                                    []
                                |> List.reverse
                    in
                    [ Heading H2 [ Text "1" ]
                    , Heading H2 [ Text "2" ]
                    , BlockQuote
                        [ Heading H2 [ Text "3A" ]
                        , BlockQuote
                            [ Heading H2 [ Text "3Ai" ]
                            , Heading H2 [ Text "3Aii" ]
                            ]
                        , Heading H2 [ Text "3B" ]
                        ]
                    , Heading H2 [ Text "4" ]
                    ]
                        |> gatherHeadingText
                        |> Expect.equal
                            [ "1", "2", "3A", "3Ai", "3Aii", "3B", "4" ]
            , test "add slugs" <|
                \() ->
                    let
                        gatherHeadingOccurrences : List Block -> ( Dict.Dict String Int, List (BlockWithMeta (Maybe String)) )
                        gatherHeadingOccurrences =
                            Block.mapAccuml
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
                        |> gatherHeadingOccurrences
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


type BlockWithMeta meta
    = BlockWithMeta Block meta
