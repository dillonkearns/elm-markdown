module HtmlTests exposing (suite)

import Dict
import Expect exposing (Expectation)
import Markdown.InlineParser
import Parser
import Parser.Advanced as Advanced
import Test exposing (..)
import XmlParser


type alias Parser a =
    Advanced.Parser String Parser.Problem a


suite : Test
suite =
    describe "inline parsing"
        [ test "simple div" <|
            \() ->
                """<div></div>"""
                    |> expectHtml (XmlParser.Element "div" [] [])
        ]


expectHtml : XmlParser.Node -> String -> Expectation
expectHtml expected input =
    input
        |> Advanced.run XmlParser.element
        |> Expect.equal (Ok expected)
