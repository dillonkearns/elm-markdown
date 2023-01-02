module BlockTransformTests exposing (suite)

import Dict
import Expect
import GithubSlugs
import Markdown.Block as Block exposing (Block(..), HeadingLevel(..), Html(..), Inline(..))
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
                    |> List.map
                        (Block.walkInlines
                            (\inline ->
                                case inline of
                                    Link destination title inlines ->
                                        Link (httpLinksToHttps destination) title inlines

                                    _ ->
                                        inline
                            )
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
        , test "walk" <|
            \() ->
                let
                    bumpHeadingLevel : HeadingLevel -> HeadingLevel
                    bumpHeadingLevel level =
                        case level of
                            H1 ->
                                H2

                            H2 ->
                                H3

                            H3 ->
                                H4

                            H4 ->
                                H5

                            H5 ->
                                H6

                            H6 ->
                                H6
                in
                [ Heading H1 [ Text "First heading" ]
                , Paragraph [ Text "Paragraph" ]
                , BlockQuote
                    [ Heading H2 [ Text "Paragraph" ]
                    ]
                , Heading H1 [ Text "Second heading" ]
                ]
                    |> List.map
                        (Block.walk
                            (\block ->
                                case block of
                                    Heading level children ->
                                        Heading (bumpHeadingLevel level) children

                                    _ ->
                                        block
                            )
                        )
                    |> Expect.equal
                        [ Heading H2 [ Text "First heading" ]
                        , Paragraph [ Text "Paragraph" ]
                        , BlockQuote
                            [ Heading H3 [ Text "Paragraph" ]
                            ]
                        , Heading H2 [ Text "Second heading" ]
                        ]
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
                        , HtmlBlock <|
                            HtmlElement "my-element"
                                []
                                [ Heading H2 [ Text "3Aiii" ]
                                , Heading H2 [ Text "3Aiv" ]
                                ]
                        ]
                    , Heading H2 [ Text "3B" ]
                    ]
                , Heading H2 [ Text "4" ]
                ]
                    |> gatherHeadingText
                    |> Expect.equal
                        [ "1", "2", "3A", "3Ai", "3Aii", "3Aiii", "3Aiv", "3B", "4" ]
        , test "check that anchor links reference headings" <|
            \() ->
                let
                    gatherHeadingOccurrences : List Block -> ( Dict.Dict String Int, List ( Block, Maybe String ) )
                    gatherHeadingOccurrences =
                        Block.mapAndAccumulate
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
                                        , ( Heading level inlines, Just (inlineText ++ occurenceModifier) )
                                        )

                                    _ ->
                                        ( soFar
                                        , ( block, Nothing )
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
                        , [ ( Heading H1 [ Text "foo" ], Just "foo" )
                          , ( Heading H1 [ Text "bar" ], Just "bar" )
                          , ( Heading H1 [ Text "foo" ], Just "foo1" )
                          ]
                        )
        , test "add slugs" <|
            \() ->
                let
                    slugs : List Block -> List String
                    slugs blocks =
                        blocks
                            |> GithubSlugs.gatherHeadingOccurrences
                            |> List.filterMap Tuple.second

                    allLinks : List Block -> List String
                    allLinks blocks =
                        blocks
                            |> Block.foldl
                                (\block acc ->
                                    case block of
                                        Block.Paragraph inlines ->
                                            (inlines |> List.filterMap extractLinkDestinations) ++ acc

                                        _ ->
                                            acc
                                )
                                []

                    extractLinkDestinations : Inline -> Maybe String
                    extractLinkDestinations inline =
                        case inline of
                            Link destination _ _ ->
                                Just destination

                            _ ->
                                Nothing

                    checkLinkReferences : List Block -> Result { missingRefs : List String } value
                    checkLinkReferences blocks =
                        let
                            destinations : List String
                            destinations =
                                allLinks blocks
                        in
                        Err
                            { missingRefs =
                                destinations
                                    |> List.filter
                                        (\destination ->
                                            (slugs blocks |> List.map (\slug -> "#" ++ slug))
                                                |> List.member destination
                                                |> not
                                        )
                            }
                in
                [ Paragraph
                    [ Link "#getting-started" Nothing [ Text "see the getting started section." ]
                    , Link "#resources" Nothing [ Text "Check out the resources" ]
                    ]
                , Heading H2 [ Text "getting-started" ]
                ]
                    |> checkLinkReferences
                    |> Expect.equal (Err { missingRefs = [ "#resources" ] })
        , test "extract text from HTML block" <|
            \() ->
                [ HtmlInline
                    (HtmlElement "greeting"
                        []
                        [ [ Strong [ Text "Hello" ]
                          , Text " there!"
                          ]
                            |> Paragraph
                        ]
                    )
                , HtmlInline
                    (HtmlElement "greeting"
                        []
                        [ [ Strong [ Text "Buenos" ]
                          , Text " dias!"
                          ]
                            |> Paragraph
                        ]
                    )
                ]
                    |> Block.extractInlineText
                    |> Expect.equal "Hello there!Buenos dias!"
        ]
