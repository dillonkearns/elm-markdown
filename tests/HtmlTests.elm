module HtmlTests exposing (suite)

import Expect exposing (Expectation)
import HtmlParser
import Parser.Advanced as Advanced
import Test exposing (..)


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
        , test "doctype declaration" <|
            \() ->
                """<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN"
"http://www.w3.org/TR/html4/strict.dtd">"""
                    |> expectHtml (HtmlParser.Declaration "DOCTYPE" """HTML PUBLIC "-//W3C//DTD HTML 4.01//EN"
"http://www.w3.org/TR/html4/strict.dtd\"""")
        , test "processing instruction" <|
            \() ->
                """<?php

  echo '>';

?>"""
                    |> expectHtml (HtmlParser.ProcessingInstruction "php\n\n  echo '>';\n\n")
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
        , describe "unclosed tags do not cause infinite loops"
            [ test "cdata" <|
                \() ->
                    """<![CDATA[Whoops, I forgot the closing >.]]"""
                        |> expectError
            , test "HTML element" <|
                \() ->
                    """<div> Whoops, I forgot the closing tag<"""
                        |> expectError
            , test "processing instruction" <|
                \() ->
                    """<? Whoops, I forgot the closing tag ?"""
                        |> expectError
            ]
        ]


expectHtml : HtmlParser.Node -> String -> Expectation
expectHtml expected input =
    input
        |> Advanced.run HtmlParser.html
        |> Expect.equal (Ok expected)


expectError : String -> Expectation
expectError input =
    case input |> Advanced.run HtmlParser.html of
        Ok _ ->
            Expect.fail "Expecting an error."

        Err _ ->
            Expect.pass
