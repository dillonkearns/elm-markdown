module LinkReferenceDefinitionTests exposing (suite)

import Expect
import Markdown.LinkReferenceDefinition as LinkReferenceDefinition
import Parser.Advanced as Advanced
import Test exposing (..)


suite : Test
suite =
    describe "link reference definitions"
        [ test "basic definition with title" <|
            \() ->
                """[foo]: /url "title"
"""
                    |> expectParseTo
                        ( "foo", { destination = "/url", title = Just "title" } )
        , test "single-quoted title with link in angle brackets" <|
            \() ->
                """[Foo bar]:
<my url>
'title'
"""
                    |> expectParseTo
                        ( "Foo bar", { destination = "my%20url", title = Just "title" } )
        ]


expectParseTo :
    LinkReferenceDefinition.LinkReferenceDefinition
    -> String
    -> Expect.Expectation
expectParseTo expected input =
    input
        |> Advanced.run LinkReferenceDefinition.parser
        |> Expect.equal (Ok expected)
