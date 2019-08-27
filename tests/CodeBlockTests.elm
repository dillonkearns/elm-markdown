module CodeBlockTests exposing (suite)

import Expect
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
        ]


unstyledText : String -> List Markdown.Inlines.StyledString
unstyledText body =
    [ { string = body, style = { isCode = False, isBold = False, isItalic = False, link = Nothing } } ]


parserError : String -> Expect.Expectation
parserError markdown =
    case parse markdown of
        Ok _ ->
            Expect.fail "Expected a parser failure!"

        Err _ ->
            Expect.pass
