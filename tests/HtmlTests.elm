module HtmlTests exposing (suite)

import Dict
import Expect exposing (Expectation)
import HtmlParser
import Markdown.InlineParser
import Parser
import Parser.Advanced as Advanced
import Test exposing (..)


type alias Parser a =
    Advanced.Parser String Parser.Problem a


suite : Test
suite =
    describe "inline parsing"
        [ test "simple div" <|
            \() ->
                """<div></div>"""
                    |> expectHtml (HtmlParser.Element "div" [] [])
        , test "empty comment" <|
            \() ->
                """<!---->"""
                    |> expectHtml (HtmlParser.Comment "")
        , test "simple comment" <|
            \() ->
                """<!-- hello! -->"""
                    |> expectHtml (HtmlParser.Comment " hello! ")
        , test "CDATA" <|
            \() ->
                """<![CDATA[This is CDATA! :-)]]>"""
                    |> expectHtml (HtmlParser.Cdata "This is CDATA! :-)")
        , test "CDATA with nested HTML" <|
            \() ->
                """<![CDATA[<raw-html />]]>"""
                    |> expectHtml (HtmlParser.Cdata "<raw-html />")
        , test "unclosed cdata" <|
            \() ->
                """<![CDATA[Whoops, I forgot the closing >.]]"""
                    |> expectError (HtmlParser.Cdata "<raw-html />")
        , test "multi-line comment" <|
            \() ->
                """<!--
hello!
next line
-->"""
                    |> expectHtml (HtmlParser.Comment "\nhello!\nnext line\n")
        , test "nested HTML" <|
            \() ->
                """<Resources>

<Book title="Crime and Punishment" />


</Resources>
                """
                    |> expectHtml
                        (HtmlParser.Element "resources"
                            []
                            [ HtmlParser.Text "\n\n"
                            , HtmlParser.Element "book" [ { name = "title", value = "Crime and Punishment" } ] []
                            , HtmlParser.Text "\n\n\n"
                            ]
                        )
        , test "comments within nested HTML" <|
            \() ->
                """<Resources>

<Book title="Crime and Punishment">
  <!-- this is the book review -->
  This is my review...
</Book>


</Resources>
                """
                    |> expectHtml
                        (HtmlParser.Element "resources"
                            []
                            [ HtmlParser.Text "\n\n"
                            , HtmlParser.Element "book"
                                [ { name = "title", value = "Crime and Punishment" } ]
                                [ HtmlParser.Text "\n  "
                                , HtmlParser.Comment " this is the book review "
                                , HtmlParser.Text "\n  This is my review...\n"
                                ]
                            , HtmlParser.Text "\n\n\n"
                            ]
                        )
        ]


expectHtml : HtmlParser.Node -> String -> Expectation
expectHtml expected input =
    input
        |> Advanced.run HtmlParser.element
        |> Expect.equal (Ok expected)


expectError : HtmlParser.Node -> String -> Expectation
expectError expected input =
    case input |> Advanced.run HtmlParser.element of
        Ok _ ->
            Expect.fail "Expecting an error."

        Err error ->
            Expect.pass
