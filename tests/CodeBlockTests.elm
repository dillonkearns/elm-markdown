module CodeBlockTests exposing (suite)

import Expect
import Markdown.Block as Block exposing (Inline, InlineStyle)
import Markdown.CodeBlock
import Markdown.Inlines
import Markdown.Parser exposing (..)
import Parser.Advanced as Advanced
import Test exposing (..)


suite : Test
suite =
    describe "list parsing"
        [ test "basic list" <|
            \() ->
                """```
theAnswer = 42
```"""
                    |> Advanced.run Markdown.CodeBlock.parser
                    |> Expect.equal
                        (Ok
                            { body = "theAnswer = 42", language = Nothing }
                        )
        , test "code fence with language" <|
            \() ->
                """```shell
$ echo hello world
```"""
                    |> Advanced.run Markdown.CodeBlock.parser
                    |> Expect.equal
                        (Ok
                            { body = "$ echo hello world"
                            , language = Just "shell"
                            }
                        )
        , test "code fence can use ~~~ as delimeter" <|
            \() ->
                """~~~shell
$ echo hello world
~~~"""
                    |> Advanced.run Markdown.CodeBlock.parser
                    |> Expect.equal
                        (Ok
                            { body = "$ echo hello world"
                            , language = Just "shell"
                            }
                        )
        ]


unstyledText : String -> List Inline
unstyledText body =
    [ { string = body, style = { isCode = False, isBold = False, isItalic = False, link = Nothing } } ]


parserError : String -> Expect.Expectation
parserError markdown =
    case parse markdown of
        Ok _ ->
            Expect.fail "Expected a parser failure!"

        Err _ ->
            Expect.pass
