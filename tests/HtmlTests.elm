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
        , test "multi-line comment" <|
            \() ->
                """<!--
hello!
next line
-->"""
                    |> expectHtml (HtmlParser.Comment "\nhello!\nnext line\n")
        ]


expectHtml : HtmlParser.Node -> String -> Expectation
expectHtml expected input =
    input
        |> Advanced.run HtmlParser.element
        |> Expect.equal (Ok expected)
