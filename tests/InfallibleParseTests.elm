module InfallibleParseTests exposing (suite)

import Expect
import Fuzz
import Markdown.Parser
import Test exposing (..)


suite : Test
suite =
    describe "infallible parse"
        [ fuzz Fuzz.string "arbitrary strings always parse" <|
            \input ->
                Markdown.Parser.parse input
                    |> (\_ -> Expect.pass)
        , fuzz Fuzz.string "parse always returns a list (length >= 0)" <|
            \input ->
                Markdown.Parser.parse input
                    |> List.length
                    |> Expect.atLeast 0
        , test "deeply nested block quotes" <|
            \() ->
                "> > > > > deeply nested"
                    |> Markdown.Parser.parse
                    |> (\_ -> Expect.pass)
        , test "unterminated code fence" <|
            \() ->
                "```\nunclosed code"
                    |> Markdown.Parser.parse
                    |> (\_ -> Expect.pass)
        , test "unterminated HTML tag" <|
            \() ->
                "<div> unclosed"
                    |> Markdown.Parser.parse
                    |> (\_ -> Expect.pass)
        , test "mixed interleaved syntax" <|
            \() ->
                "# heading\n> *bold **nested\n```\ncode\n- list\n[link](url"
                    |> Markdown.Parser.parse
                    |> (\_ -> Expect.pass)
        , test "empty string" <|
            \() ->
                ""
                    |> Markdown.Parser.parse
                    |> Expect.equal []
        , test "only whitespace" <|
            \() ->
                "   \n  \n  "
                    |> Markdown.Parser.parse
                    |> Expect.equal []
        , test "very long line" <|
            \() ->
                String.repeat 10000 "a"
                    |> Markdown.Parser.parse
                    |> (\_ -> Expect.pass)
        , test "control characters" <|
            \() ->
                "\u{0000}\u{0001}\u{0002}\u{0003}"
                    |> Markdown.Parser.parse
                    |> (\_ -> Expect.pass)
        , fuzz (Fuzz.stringOfLengthBetween 0 200) "random strings up to 200 chars" <|
            \input ->
                Markdown.Parser.parse input
                    |> (\_ -> Expect.pass)
        , fuzz markdownHotspotStringFuzzer "markdown-biased characters always parse" <|
            \input ->
                Markdown.Parser.parse input
                    |> (\_ -> Expect.pass)
        , fuzz markdownFragmentFuzzer "markdown fragment strings always parse" <|
            \input ->
                Markdown.Parser.parse input
                    |> (\_ -> Expect.pass)
        , fuzz markdownDocumentFuzzer "markdown document-like strings always parse" <|
            \input ->
                Markdown.Parser.parse input
                    |> (\_ -> Expect.pass)
        ]



-- Layer 1: Character-level hot spots


markdownHotspotCharFuzzer : Fuzz.Fuzzer Char
markdownHotspotCharFuzzer =
    Fuzz.frequency
        [ ( 3
          , Fuzz.oneOfValues
                [ '#', '*', '_', '-', '+', '`', '>', '[', ']', '(', ')', '<', '\\', '~', '|', '.', '=', '!', '&' ]
          )
        , ( 2, Fuzz.oneOfValues [ ' ', '\n', '\t' ] )
        , ( 1
          , Fuzz.intRange (Char.toCode '0') (Char.toCode '9')
                |> Fuzz.map Char.fromCode
          )
        , ( 1
          , Fuzz.oneOf
                [ Fuzz.intRange (Char.toCode 'a') (Char.toCode 'z') |> Fuzz.map Char.fromCode
                , Fuzz.intRange (Char.toCode 'A') (Char.toCode 'Z') |> Fuzz.map Char.fromCode
                ]
          )
        , ( 0.5
          , Fuzz.intRange 32 126 |> Fuzz.map Char.fromCode
          )
        ]


markdownHotspotStringFuzzer : Fuzz.Fuzzer String
markdownHotspotStringFuzzer =
    Fuzz.listOfLengthBetween 1 80 markdownHotspotCharFuzzer
        |> Fuzz.map String.fromList



-- Layer 2: Fragment-level hot spots


markdownFragmentFuzzer : Fuzz.Fuzzer String
markdownFragmentFuzzer =
    Fuzz.listOfLengthBetween 1 20 fragmentPieceFuzzer
        |> Fuzz.map String.concat


fragmentPieceFuzzer : Fuzz.Fuzzer String
fragmentPieceFuzzer =
    Fuzz.oneOf
        [ -- Block-level prefixes
          Fuzz.oneOfValues
            [ "# ", "## ", "### ", "> ", "- ", "* ", "+ ", "1. ", "```", "---", "===", "| ", "    " ]

        -- Inline delimiters
        , Fuzz.oneOfValues
            [ "*", "**", "_", "__", "~~", "`", "[", "](", ")", "![", "<", ">", "</" ]

        -- HTML fragments
        , Fuzz.oneOfValues
            [ "<div>", "</div>", "<!--", "-->", "<![CDATA[", "]]>", "<?", "?>", "<!", "/>" ]

        -- Short run of hotspot characters
        , Fuzz.listOfLengthBetween 1 5 markdownHotspotCharFuzzer
            |> Fuzz.map String.fromList

        -- Plain word (1-5 alpha chars)
        , Fuzz.listOfLengthBetween 1 5
            (Fuzz.oneOf
                [ Fuzz.intRange (Char.toCode 'a') (Char.toCode 'z') |> Fuzz.map Char.fromCode
                , Fuzz.intRange (Char.toCode 'A') (Char.toCode 'Z') |> Fuzz.map Char.fromCode
                ]
            )
            |> Fuzz.map String.fromList

        -- Sometimes add a newline separator
        , Fuzz.constant "\n"
        ]



-- Layer 3: Line-level structure (document-like)


markdownDocumentFuzzer : Fuzz.Fuzzer String
markdownDocumentFuzzer =
    Fuzz.listOfLengthBetween 1 10 lineFuzzer
        |> Fuzz.map (String.join "\n")


lineFuzzer : Fuzz.Fuzzer String
lineFuzzer =
    Fuzz.frequency
        [ ( 3, markdownFragmentFuzzer )
        , ( 2, blockPrefixedLineFuzzer )
        , ( 1, Fuzz.constant "" )
        ]


blockPrefixedLineFuzzer : Fuzz.Fuzzer String
blockPrefixedLineFuzzer =
    Fuzz.map2
        (\prefix content -> prefix ++ content)
        (Fuzz.oneOfValues
            [ "# ", "## ", "### ", "> ", "- ", "* ", "+ ", "1. ", "```", "    " ]
        )
        markdownFragmentFuzzer
