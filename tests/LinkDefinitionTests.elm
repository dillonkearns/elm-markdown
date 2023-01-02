module LinkDefinitionTests exposing (suite)

import Expect exposing (Expectation)
import Markdown.Block exposing (..)
import Markdown.Parser
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "definition nested 1 level"
        [ test "definitions" <|
            \() ->
                """[foo]

> [foo]: /url"""
                    |> expectResolvedLinkReference
        , test "definition nested 2 levels" <|
            \() ->
                """[foo]
                    
> > [foo]: /url"""
                    |> expectResolvedLinkReference
        , test "definition nested 3 levels" <|
            \() ->
                """[foo]


> > > [foo]: /url
"""
                    |> expectResolvedLinkReference
        ]


expectResolvedLinkReference : String -> Expectation
expectResolvedLinkReference markdownString =
    markdownString
        |> Markdown.Parser.parse
        |> Result.map List.head
        |> Expect.equal (Ok (Just <| Paragraph [ Link "/url" Nothing [ Text "foo" ] ]))
