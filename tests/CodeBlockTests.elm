module CodeBlockTests exposing (suite)

import Expect
import Markdown.CodeBlock
import Parser.Advanced as Advanced
import Test exposing (..)


suite : Test
suite =
    describe "list parsing"
        [ test "basic list" <|
            \() ->
                """```
theAnswer = 42
theQuestion = 42
```"""
                    |> Advanced.run Markdown.CodeBlock.parser
                    |> Expect.equal
                        (Ok
                            { body = "theAnswer = 42\ntheQuestion = 42\n", language = Nothing }
                        )
        , test "basic list (carriage returns)" <|
            \() ->
                """```\u{000D}theAnswer = 42\u{000D}theQuestion = 42\u{000D}```"""
                    |> Advanced.run Markdown.CodeBlock.parser
                    |> Expect.equal
                        (Ok
                            { body = "theAnswer = 42\u{000D}theQuestion = 42\u{000D}", language = Nothing }
                        )
        , test "code fence with language" <|
            \() ->
                """```shell
$ echo hello world
```"""
                    |> Advanced.run Markdown.CodeBlock.parser
                    |> Expect.equal
                        (Ok
                            { body = "$ echo hello world\n"
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
                            { body = "$ echo hello world\n"
                            , language = Just "shell"
                            }
                        )
        , test "code fence ignores too short closing fences" <|
            \() ->
                """````
aaa
```
``````"""
                    |> Advanced.run Markdown.CodeBlock.parser
                    |> Expect.equal
                        (Ok
                            { body = "aaa\n```\n"
                            , language = Nothing
                            }
                        )
        , test "code fence works with nothing inside" <|
            \() ->
                """```

  
```"""
                    |> Advanced.run Markdown.CodeBlock.parser
                    |> Expect.equal
                        (Ok
                            { body = "\n  \n"
                            , language = Nothing
                            }
                        )
        , test "indented code block removes content indentation" <|
            \() ->
                """  ```
  aaa
```"""
                    |> Advanced.run Markdown.CodeBlock.parser
                    |> Expect.equal
                        (Ok
                            { body = "aaa\n"
                            , language = Nothing
                            }
                        )
        , test "closing fences cannot contain other characters" <|
            \() ->
                """```
aaa
```bbb
```"""
                    |> Advanced.run Markdown.CodeBlock.parser
                    |> Expect.equal
                        (Ok
                            { body = "aaa\n```bbb\n"
                            , language = Nothing
                            }
                        )
        , test "fenced code blocks are truncated by string ends (one line)" <|
            \() ->
                """```shell"""
                    |> Advanced.run Markdown.CodeBlock.parser
                    |> Expect.equal
                        (Ok
                            { body = ""
                            , language = Just "shell"
                            }
                        )
        , test "fenced code blocks are truncated by string ends" <|
            \() ->
                """```
aaa
"""
                    |> Advanced.run Markdown.CodeBlock.parser
                    |> Expect.equal
                        (Ok
                            { body = "aaa\n"
                            , language = Nothing
                            }
                        )
        ]
