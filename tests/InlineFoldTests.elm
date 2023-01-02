module InlineFoldTests exposing (suite)

import Dict exposing (Dict)
import Expect
import Markdown.Block as Block exposing (Block(..), HeadingLevel(..), Html(..), Inline(..), ListItem(..), Task(..))
import Test exposing (..)


allInlines : List Inline
allInlines =
    [ HtmlInline (HtmlElement "" [] [ Paragraph [ Text "htmlinlineparablock" ] ])
    , Link "" Nothing [ Text "LinkText" ]
    , Image "" Nothing [ Text "ImageText" ]
    , Emphasis [ Text "EmphasisText" ]
    , Strikethrough [ Text "StrikethroughText" ]
    , Strong [ Text "StrongText" ]
    , CodeSpan ""
    , Text "TextText"
    , HardLineBreak
    ]


allBlocks : List Block
allBlocks =
    [ HtmlBlock (HtmlElement "" [] [ Paragraph [ Text "htmlblockparablock" ] ])
    , UnorderedList Block.Loose <| List.map (\i -> ListItem NoTask [ Paragraph [ i ] ]) allInlines
    , OrderedList Block.Loose 0 [ [ Paragraph allInlines ] ]
    , BlockQuote [ Paragraph allInlines ]
    , Heading H1 allInlines
    , Paragraph allInlines
    , Table [ { label = allInlines, alignment = Nothing } ] [ [ allInlines ] ]
    , CodeBlock { body = "", language = Nothing }
    , ThematicBreak
    ]


countInline : Inline -> Dict String Int -> Dict String Int
countInline inline totals =
    let
        increment : Dict String Int -> String -> Dict String Int
        increment =
            \dict str ->
                case Dict.get str dict of
                    Just t ->
                        Dict.insert str (t + 1) totals

                    Nothing ->
                        Dict.insert str 1 totals
    in
    case inline of
        Text what ->
            increment totals what

        HardLineBreak ->
            increment totals "HardLineBreak"

        CodeSpan _ ->
            increment totals "CodeSpan"

        Link _ _ _ ->
            increment totals "Link"

        Image _ _ _ ->
            increment totals "Image"

        HtmlInline _ ->
            increment totals "HtmlInline"

        Emphasis _ ->
            increment totals "Emphasis"

        Strikethrough _ ->
            increment totals "Strikethrough"

        Strong _ ->
            increment totals "Strong"


suite : Test
suite =
    describe "fold inlines"
        [ test "count inlines" <|
            \() ->
                let
                    totes : Dict String Int
                    totes =
                        Block.inlineFoldl countInline Dict.empty allBlocks

                    occs : Int
                    occs =
                        7

                    targets : Dict String Int
                    targets =
                        Dict.fromList
                            [ ( "HardLineBreak", occs )
                            , ( "CodeSpan", occs )
                            , ( "Link", occs )
                            , ( "Image", occs )
                            , ( "HtmlInline", occs )
                            , ( "Emphasis", occs )
                            , ( "Strong", occs )
                            , ( "ImageText", occs )
                            , ( "EmphasisText", occs )
                            , ( "Strikethrough", occs )
                            , ( "StrikethroughText", occs )
                            , ( "StrongText", occs )
                            , ( "TextText", occs )
                            , ( "LinkText", occs )
                            , ( "htmlblockparablock", 1 )
                            , ( "htmlinlineparablock", occs )
                            ]
                in
                Expect.equal totes targets
        ]
