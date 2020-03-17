module CodeBlockTests exposing (suite)

import Expect
import Markdown.CodeBlock
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
