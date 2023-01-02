module SetextHeadingTests exposing (suite)

import Expect
import Markdown.Block as Block exposing (..)
import Markdown.Parser
import Parser
import Parser.Advanced as Advanced
import Test exposing (..)


parse : String -> Result (List (Advanced.DeadEnd String Parser.Problem)) (List Block)
parse =
    Markdown.Parser.parse


suite : Test
suite =
    describe "setext headings"
        [ test "h1 and h2 headings" <|
            \() ->
                """Foo
-------------------------

Foo
=
"""
                    |> parse
                    |> Expect.equal
                        (Ok
                            [ Block.Heading H2 [ Text "Foo" ]
                            , Block.Heading H1 [ Text "Foo" ]
                            ]
                        )
        , test "whitespace after setext line" <|
            \() ->
                """
Foo
   ----     
"""
                    |> parse
                    |> Expect.equal
                        (Ok
                            [ Block.Heading H2 [ Text "Foo" ]
                            ]
                        )
        , test "inconsistent setext line" <|
            \() ->
                """Foo
--==
"""
                    |> parse
                    |> Expect.equal
                        (Ok
                            [ Block.Paragraph [ Text "Foo\n--==" ]
                            ]
                        )
        ]
