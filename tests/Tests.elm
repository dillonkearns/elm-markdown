module Tests exposing (suite)

import Expect
import Markdown.Block as Block exposing (..)
import Markdown.Parser as Markdown
import Test exposing (..)


parse : String -> List Block
parse markdown =
    markdown
        |> Markdown.parse


suite : Test
suite =
    describe "parsing"
        [ describe "headings"
            [ test "Heading 1" <|
                \() ->
                    "# Hello!"
                        |> parse
                        |> Expect.equal [ Block.Heading Block.H1 (unstyledText "Hello!") ]
            , test "heading can end with trailing #'s'" <|
                \() ->
                    "# Hello! ###"
                        |> parse
                        |> Expect.equal [ Block.Heading Block.H1 (unstyledText "Hello!") ]
            , test "Heading 2" <|
                \() ->
                    "## Hello!"
                        |> parse
                        |> Expect.equal [ Block.Heading Block.H2 (unstyledText "Hello!") ]
            , test "Emphasis line is not interpreted as a list" <|
                \() ->
                    "*This is not a list, it's a paragraph with emphasis*\n"
                        |> parse
                        |> Expect.equal [ Block.Paragraph (emphasisText "This is not a list, it's a paragraph with emphasis") ]
            , test "Line starting with a decimal is not interpreted as a list" <|
                \() ->
                    "3.5 is a number - is not a list\n"
                        |> parse
                        |> Expect.equal [ Block.Paragraph (unstyledText "3.5 is a number - is not a list") ]
            , test "Heading 7 is parsed using fallback parsing" <|
                \() ->
                    "####### Hello!"
                        |> parse
                        |> Expect.equal [ Block.Paragraph [ Text "####### Hello!" ] ]
            ]
        , test "plain text" <|
            \() ->
                "This is just some text"
                    |> parse
                    |> Expect.equal [ Block.Paragraph (unstyledText "This is just some text") ]
        , test "parse heading then plain text" <|
            \() ->
                """# Heading
This is just some text
"""
                    |> parse
                    |> Expect.equal
                        [ Block.Heading Block.H1 (unstyledText "Heading")
                        , Block.Paragraph (unstyledText "This is just some text")
                        ]
        , test "doesn't need to end in newline" <|
            \() ->
                """# Heading
This is just some text"""
                    |> parse
                    |> Expect.equal
                        [ Block.Heading Block.H1 (unstyledText "Heading")
                        , Block.Paragraph (unstyledText "This is just some text")
                        ]
        , test "long example" <|
            \() ->
                """# Heading

This is just some text.

## Subheading

Body of the subheading.
"""
                    |> parse
                    |> Expect.equal
                        [ Block.Heading Block.H1 (unstyledText "Heading")
                        , Block.Paragraph (unstyledText "This is just some text.")
                        , Block.Heading Block.H2 (unstyledText "Subheading")
                        , Block.Paragraph (unstyledText "Body of the subheading.")
                        ]
        , test "embedded HTML" <|
            \() ->
                """# Heading
<div>
Hello!
</div>
"""
                    |> parse
                    |> Expect.equal
                        [ Block.Heading Block.H1 (unstyledText "Heading")
                        , Block.HtmlBlock
                            (Block.HtmlElement "div"
                                []
                                [ Block.Paragraph (unstyledText "Hello!")
                                ]
                                "\nHello!\n"
                            )
                        ]
        , test "embedded HTML with attribute containing <> chars" <|
            \() ->
                """# Heading
<div attr="<u>">
Hello!
</div>
"""
                    |> parse
                    |> Expect.equal
                        [ Block.Heading Block.H1 (unstyledText "Heading")
                        , Block.HtmlBlock
                            (Block.HtmlElement "div"
                                [ { name = "attr", value = "<u>" } ]
                                [ Block.Paragraph (unstyledText "Hello!")
                                ]
                                "\nHello!\n"
                            )
                        ]
        , test "heading within HTML" <|
            \() ->
                """# Heading
<div>
# Heading in a div!

</div>
"""
                    |> parse
                    |> Expect.equal
                        [ Block.Heading Block.H1 (unstyledText "Heading")
                        , Block.HtmlBlock
                            (Block.HtmlElement "div"
                                []
                                [ Block.Heading Block.H1 (unstyledText "Heading in a div!")
                                ]
                                "\n# Heading in a div!\n\n"
                            )
                        ]
        , test "simple list" <|
            \() ->
                """- One
- Two
- Three
"""
                    |> parse
                    |> Expect.equal
                        [ Block.UnorderedList Block.Tight
                            [ plainListItem "One"
                            , plainListItem "Two"
                            , plainListItem "Three"
                            ]

                        -- TODO why is this extra block here? Fix
                        -- , ListBlock []
                        ]
        , test "sibling unordered lists with different markers" <|
            \() ->
                """- Item 1
- Item 2
- Item 3
+ Item 4
+ Item 5
+ Item 6
* Item 7
* Item 8
* Item 9
"""
                    |> parse
                    |> Expect.equal
                        [ Block.UnorderedList Block.Tight
                            [ plainListItem "Item 1"
                            , plainListItem "Item 2"
                            , plainListItem "Item 3"
                            ]
                        , Block.UnorderedList Block.Tight
                            [ plainListItem "Item 4"
                            , plainListItem "Item 5"
                            , plainListItem "Item 6"
                            ]
                        , Block.UnorderedList Block.Tight
                            [ plainListItem "Item 7"
                            , plainListItem "Item 8"
                            , plainListItem "Item 9"
                            ]
                        ]
        , test "sibling ordered lists with different markers" <|
            \() ->
                """1. foo
2. bar
3) baz
"""
                    |> parse
                    |> Expect.equal
                        [ Block.OrderedList Block.Tight
                            1
                            [ [ Paragraph (unstyledText "foo") ]
                            , [ Paragraph (unstyledText "bar") ]
                            ]
                        , Block.OrderedList Block.Tight
                            3
                            [ [ Paragraph (unstyledText "baz") ]
                            ]
                        ]
        , test "A paragraph with a numeral that is NOT 1 in the text before a blank line" <|
            \() ->
                """The number of windows in my house is
14.  The number of doors is 6."""
                    |> parse
                    |> Expect.equal
                        [ Block.Paragraph (unstyledText "The number of windows in my house is\n14.  The number of doors is 6.")
                        ]
        , test "A paragraph with a numeral that IS 1 in the text" <|
            \() ->
                """The number of windows in my house is
1.  The number of doors is 6.
"""
                    |> parse
                    |> Expect.equal
                        [ Block.Paragraph (unstyledText "The number of windows in my house is")
                        , Block.OrderedList Block.Tight
                            1
                            [ [ Paragraph (unstyledText "The number of doors is 6.") ] ]
                        ]
        , test "thematic break" <|
            \() ->
                """---"""
                    |> parse
                    |> Expect.equal
                        [ Block.ThematicBreak
                        ]
        , test "simple table" <|
            \() ->
                """| abc | def |
|---|---|
"""
                    |> parse
                    |> Expect.equal
                        [ Block.Table
                            [ { label = [ Text "abc" ], alignment = Nothing }
                            , { label = [ Text "def" ], alignment = Nothing }
                            ]
                            []
                        ]
        , test "simple table with data" <|
            \() ->
                """| abc | def |
|---|---|
| foo | bar |
| bar | baz |
"""
                    |> parse
                    |> Expect.equal
                        [ Block.Table
                            [ { label = [ Text "abc" ], alignment = Nothing }
                            , { label = [ Text "def" ], alignment = Nothing }
                            ]
                            [ [ [ Text "foo" ], [ Text "bar" ] ]
                            , [ [ Text "bar" ], [ Text "baz" ] ]
                            ]
                        ]
        , test "table with alignment" <|
            \() ->
                """| abc | def | ghi | jkl
|:---|:------:|--:|---|
| foo | bar | baz | boo |
| bar | baz | boo | foo |
"""
                    |> parse
                    |> Expect.equal
                        [ Block.Table
                            [ { label = [ Text "abc" ], alignment = Just AlignLeft }
                            , { label = [ Text "def" ], alignment = Just AlignCenter }
                            , { label = [ Text "ghi" ], alignment = Just AlignRight }
                            , { label = [ Text "jkl" ], alignment = Nothing }
                            ]
                            [ [ [ Text "foo" ], [ Text "bar" ], [ Text "baz" ], [ Text "boo" ] ]
                            , [ [ Text "bar" ], [ Text "baz" ], [ Text "boo" ], [ Text "foo" ] ]
                            ]
                        ]
        , test "table with a cell that looks like a heading but isn't" <|
            \() ->
                """| abc | def |
| --- | --- |
| bar | baz |
####### asdf
"""
                    |> parse
                    |> Expect.equal
                        [ Block.Table
                            [ { label = [ Text "abc" ], alignment = Nothing }
                            , { label = [ Text "def" ], alignment = Nothing }
                            ]
                            [ [ [ Text "bar" ], [ Text "baz" ] ]
                            , [ [ Text "####### asdf" ], [] ]
                            ]
                        ]
        , test "table ended by a heading" <|
            \() ->
                """| abc | def |
| --- | --- |
| bar | baz |
###### asdf
"""
                    |> parse
                    |> Expect.equal
                        [ Block.Table
                            [ { label = [ Text "abc" ], alignment = Nothing }
                            , { label = [ Text "def" ], alignment = Nothing }
                            ]
                            [ [ [ Text "bar" ], [ Text "baz" ] ]
                            ]
                        , Block.Heading Block.H6 [ Text "asdf" ]
                        ]
        , test "tables separated by a blank line should be separate" <|
            \() ->
                """| abc | def |
| --- | --- |
| bar | baz |

| abc | def |
| --- | --- |
| bar | baz |
"""
                    |> parse
                    |> Expect.equal
                        [ Block.Table
                            [ { label = [ Text "abc" ], alignment = Nothing }
                            , { label = [ Text "def" ], alignment = Nothing }
                            ]
                            [ [ [ Text "bar" ], [ Text "baz" ] ]
                            ]
                        , Block.Table
                            [ { label = [ Text "abc" ], alignment = Nothing }
                            , { label = [ Text "def" ], alignment = Nothing }
                            ]
                            [ [ [ Text "bar" ], [ Text "baz" ] ]
                            ]
                        ]
        , test "multiple thematic breaks" <|
            \() ->
                """***
---
___"""
                    |> parse
                    |> Expect.equal
                        [ Block.ThematicBreak
                        , Block.ThematicBreak
                        , Block.ThematicBreak
                        ]
        , test "thematic break followed by newline" <|
            \() ->
                """---
"""
                    |> parse
                    |> Expect.equal
                        [ Block.ThematicBreak
                        ]
        , test "blank lines are ignored" <|
            \() ->
                " \n  \n    \n\t\n"
                    |> parse
                    |> Expect.equal []
        , test "mixed content with list" <|
            \() ->
                """# Title

- This is an item
- And so is this

Text after
"""
                    |> parse
                    |> Expect.equal
                        [ Block.Heading Block.H1 (unstyledText "Title")
                        , Block.UnorderedList Block.Tight
                            [ plainListItem "This is an item"
                            , plainListItem "And so is this"
                            ]
                        , Block.Paragraph (unstyledText "Text after")

                        -- TODO why is this extra block here? Fix
                        -- , ListBlock []
                        ]
        , test "code fence with paragraph and heading below" <|
            \() ->
                """```shell
.
├── content/
├── elm.json
├── images/
├── static/
├── index.js
├── package.json
└── src/
    └── Main.elm
```

This is more stuff

## h2

qwer
"""
                    |> parse
                    |> Expect.equal
                        [ Block.CodeBlock
                            { body = ".\n├── content/\n├── elm.json\n├── images/\n├── static/\n├── index.js\n├── package.json\n└── src/\n    └── Main.elm\n"
                            , language = Just "shell"
                            }
                        , Block.Paragraph (unstyledText "This is more stuff")
                        , Block.Heading Block.H2 (unstyledText "h2")
                        , Block.Paragraph (unstyledText "qwer")
                        ]
        , test "indented code block" <|
            \() ->
                """    foo = 123"""
                    |> parse
                    |> Expect.equal [ Block.CodeBlock { body = "foo = 123", language = Nothing } ]
        , test "indented code block with tab" <|
            \() ->
                """\tfoo = 123"""
                    |> parse
                    |> Expect.equal [ Block.CodeBlock { body = "foo = 123", language = Nothing } ]
        , test "image" <|
            \() ->
                """![This is an image](/my/image.jpg)"""
                    |> parse
                    |> Expect.equal
                        [ Block.Paragraph
                            [ Block.Image "/my/image.jpg" Nothing [ Block.Text "This is an image" ]
                            ]
                        ]
        , describe "blank line"
            [ test "even though paragraphs can start with blank lines, it is not a paragraph if there are only blanks" <|
                \() ->
                    "  \n"
                        |> parse
                        |> Expect.equal []
            ]
        , describe "block quotes"
            [ test "Simple block quote" <|
                \() ->
                    ">This is a quote\n"
                        |> parse
                        |> Expect.equal [ Block.BlockQuote [ Block.Paragraph (unstyledText "This is a quote") ] ]
            , test "block quote with a space after" <|
                \() ->
                    "> This is a quote\n"
                        |> parse
                        |> Expect.equal [ Block.BlockQuote [ Block.Paragraph (unstyledText "This is a quote") ] ]
            , test "consecutive block quote lines are combined" <|
                \() ->
                    """> # Heading
> Body
"""
                        |> parse
                        |> Expect.equal
                            [ Block.BlockQuote
                                [ Block.Heading Block.H1 (unstyledText "Heading")
                                , Block.Paragraph (unstyledText "Body")
                                ]
                            ]
            , test "plain lines immediately after block quote lines are combined" <|
                \() ->
                    """> # Heading
I'm part of the block quote
"""
                        |> parse
                        |> Expect.equal
                            [ Block.BlockQuote
                                [ Block.Heading Block.H1 (unstyledText "Heading")
                                , Block.Paragraph (unstyledText "I'm part of the block quote")
                                ]
                            ]
            ]
        , test "indented code" <|
            \() ->
                """    sum a b =
      a + b
"""
                    |> parse
                    |> Expect.equal
                        [ Block.CodeBlock
                            { body = "sum a b =\n  a + b"
                            , language = Nothing
                            }
                        ]
        , test "block quotes eat the first space and allow paragraphs to start with 3 spaces" <|
            \() ->
                """>     code

>    not code
"""
                    |> parse
                    |> Expect.equal
                        [ Block.BlockQuote
                            [ Block.CodeBlock
                                { body = "code"
                                , language = Nothing
                                }
                            ]
                        , Block.BlockQuote
                            [ Block.Paragraph (unstyledText "not code")
                            ]
                        ]
        , test "inline HTML" <|
            \() ->
                """This is *italicized inline HTML <bio name="Dillon Kearns" photo="https://avatars2.githubusercontent.com/u/1384166" />*"""
                    |> parse
                    |> Expect.equal
                        [ Block.Paragraph
                            [ Block.Text "This is "
                            , Block.Emphasis
                                [ Block.Text "italicized inline HTML "
                                , Block.HtmlInline
                                    (Block.HtmlElement "bio"
                                        -- NOTE: attribute names are in reverse alphabetical order
                                        [ { name = "photo", value = "https://avatars2.githubusercontent.com/u/1384166" }
                                        , { name = "name", value = "Dillon Kearns" }
                                        ]
                                        []
                                        ""
                                    )
                                ]
                            ]
                        ]
        , test "inline HTML with an attribute with <> chars" <|
            \() ->
                """This is *italicized inline HTML <bio name="Dillon Kearns" photo="https://avatars2.githubusercontent.com/<u>/1384166" />*"""
                    |> parse
                    |> Expect.equal
                        [ Block.Paragraph
                            [ Block.Text "This is "
                            , Block.Emphasis
                                [ Block.Text "italicized inline HTML "
                                , Block.HtmlInline
                                    (Block.HtmlElement "bio"
                                        -- NOTE: attribute names are in reverse alphabetical order
                                        [ { name = "photo", value = "https://avatars2.githubusercontent.com/<u>/1384166" }
                                        , { name = "name", value = "Dillon Kearns" }
                                        ]
                                        []
                                        ""
                                    )
                                ]
                            ]
                        ]
        , test "blank lines separate paragraphs within block quote" <|
            \() ->
                """> foo
>
> bar
"""
                    |> parse
                    |> Expect.equal
                        [ Block.BlockQuote
                            [ Block.Paragraph (unstyledText "foo")
                            , Block.Paragraph (unstyledText "bar")
                            ]
                        ]
        , test "hard line break with two spaces" <|
            \() ->
                "foo  \nbaz"
                    |> parse
                    |> Expect.equal
                        [ Paragraph
                            [ Text "foo"
                            , HardLineBreak
                            , Text "baz"
                            ]
                        ]
        , test "indented code blocks cannot interrupt paragraphs" <|
            \() ->
                """aaa
                        bbb
                                                  ccc"""
                    |> parse
                    |> Expect.equal
                        [ Paragraph
                            [ Text
                                """aaa
                        bbb
                                                  ccc"""
                            ]
                        ]
        , test "keeps items grouped in a paragraph within block quotes when there are no blank lines separating them" <|
            \() ->
                """> # Foo
> bar
> baz
"""
                    |> parse
                    |> Expect.equal
                        [ Block.BlockQuote
                            [ Block.Heading Block.H1 (unstyledText "Foo")
                            , Block.Paragraph (unstyledText "bar\nbaz")
                            ]
                        ]
        , test "backslash line break" <|
            \() ->
                "Before\\\nAfter"
                    |> parse
                    |> Expect.equal
                        [ Block.Paragraph
                            [ Block.Text "Before"
                            , Block.HardLineBreak
                            , Block.Text "After"
                            ]
                        ]
        , describe "html"
            [ test "html comment" <|
                \() ->
                    "<!-- hello! -->"
                        |> parse
                        |> Expect.equal
                            [ Block.HtmlBlock (Block.HtmlComment " hello! ") ]
            , test "nested html comment" <|
                \() ->
                    """<Resources>

<Book title="Crime and Punishment">
  <!-- this is the book review -->
  This is my review...
</Book>


</Resources>
"""
                        |> parse
                        |> Expect.equal
                            [ HtmlBlock
                                (HtmlElement "resources"
                                    []
                                    [ HtmlBlock
                                        (HtmlElement "book"
                                            [ { name = "title", value = "Crime and Punishment" } ]
                                            [ HtmlBlock (HtmlComment " this is the book review ")
                                            , Paragraph [ Text "This is my review..." ]
                                            ]
                                            "\n  <!-- this is the book review -->\n  This is my review...\n"
                                        )
                                    ]
                                    "\n\n<Book title=\"Crime and Punishment\">\n  <!-- this is the book review -->\n  This is my review...\n</Book>\n\n\n"
                                )
                            ]
            ]
        , test "HTML declaration" <|
            \() ->
                """<!DOCTYPE html>"""
                    |> parse
                    |> Expect.equal
                        [ HtmlBlock (HtmlDeclaration "DOCTYPE" "html") ]
        , describe "inline html"
            [ test "cdata sections" <|
                \() ->
                    "foo <![CDATA[>&<]]>"
                        |> parse
                        |> Expect.equal
                            [ Paragraph
                                [ Text "foo "
                                , HtmlInline (Cdata ">&<")
                                ]
                            ]
            , test "nested HTML" <|
                \() ->
                    """foo <Resources><Resource type="book" title="Notes From Underground" /></Resources>"""
                        |> parse
                        |> Expect.equal
                            [ Paragraph
                                [ Text "foo "
                                , HtmlInline
                                    (HtmlElement "resources"
                                        []
                                        [ HtmlInline
                                            (HtmlElement "resource"
                                                [ { name = "type", value = "book" }
                                                , { name = "title", value = "Notes From Underground" }
                                                ]
                                                []
                                                ""
                                            )
                                        ]
                                        "<Resource type=\"book\" title=\"Notes From Underground\" />"
                                    )
                                ]
                            ]
            , test "nested markdown within nested HTML" <|
                \() ->
                    """foo <Resources><Resource type="book" title="Notes From Underground" />9/10 interesting read!</Resources>"""
                        |> parse
                        |> Expect.equal
                            [ Paragraph
                                [ Text "foo "
                                , HtmlInline
                                    (HtmlElement "resources"
                                        []
                                        [ HtmlInline
                                            (HtmlElement "resource"
                                                [ { name = "type", value = "book" }
                                                , { name = "title", value = "Notes From Underground" }
                                                ]
                                                []
                                                ""
                                            )
                                        , Text "9/10 interesting read!"
                                        ]
                                        "<Resource type=\"book\" title=\"Notes From Underground\" />9/10 interesting read!"
                                    )
                                ]
                            ]
            , test "raw body captures content for style tag" <|
                \() ->
                    "<style>\n\np { color: red; }\n\n</style>"
                        |> parse
                        |> Expect.equal
                            [ HtmlBlock
                                (HtmlElement "style"
                                    []
                                    [ Paragraph [ Text "p { color: red; }" ] ]
                                    "\n\np { color: red; }\n\n"
                                )
                            ]
            , test "raw body is empty for self-closing tags" <|
                \() ->
                    "<my-widget />"
                        |> parse
                        |> Expect.equal
                            [ HtmlBlock
                                (HtmlElement "my-widget"
                                    []
                                    []
                                    ""
                                )
                            ]
            , test "inline sup renders without paragraph wrapping" <|
                \() ->
                    "hello<sup>2</sup>world"
                        |> parse
                        |> Expect.equal
                            [ Paragraph
                                [ Text "hello"
                                , HtmlInline (HtmlElement "sup" [] [ Text "2" ] "2")
                                , Text "world"
                                ]
                            ]
            ]
        , describe "beginning with autolink"
            [ test "simple autolink" <|
                \() ->
                    "<https://elm-lang.org>\n"
                        |> parse
                        |> Expect.equal
                            [ Paragraph
                                [ Link "https://elm-lang.org" Nothing [ Text "https://elm-lang.org" ]
                                ]
                            ]
            , test "email autolink" <|
                \() ->
                    "<foo@bar.example.com>\n"
                        |> parse
                        |> Expect.equal
                            [ Paragraph
                                [ Link "mailto:foo@bar.example.com" Nothing [ Text "foo@bar.example.com" ]
                                ]
                            ]
            ]
        , describe "link reference definitions"
            [ test "basic example" <|
                \() ->
                    """[foo]: /url "title"

[foo]
"""
                        |> parse
                        |> Expect.equal
                            [ Paragraph [ Link "/url" (Just "title") [ Text "foo" ] ] ]
            , test "invalid reference uses fallback paragraph parsing" <|
                \() ->
                    """[foo]:

[foo]
"""
                        |> parse
                        |> Expect.equal
                            [ Paragraph [ Text "[foo]:" ]
                            , Paragraph [ Text "[foo]" ]
                            ]
            ]
        , describe "escaped strikethroughs"
            [ test "escaped examples 1" <|
                \() ->
                    "\\~~~Hi~~ Hello, world!"
                        |> parse
                        |> Expect.equal
                            [ Paragraph [ Text "~", Strikethrough [ Text "Hi" ], Text " Hello, world!" ] ]
            , test "escaped example 2" <|
                \() ->
                    "~~Hi~~\\~ Hello, world!"
                        |> parse
                        |> Expect.equal
                            [ Paragraph [ Strikethrough [ Text "Hi" ], Text "~ Hello, world!" ] ]
            , test "escaped example 3" <|
                \() ->
                    "~~Hi\\~~ Hello, world!"
                        |> parse
                        |> Expect.equal
                            [ Paragraph [ Text "~~Hi~~ Hello, world!" ] ]
            , test "escaped example 4" <|
                \() ->
                    "\\~\\~Hi\\~\\~ Hello, world!"
                        |> parse
                        |> Expect.equal
                            [ Paragraph [ Text "~~Hi~~ Hello, world!" ] ]
            , test "escaped example 5" <|
                \() ->
                    "\\~~Hi~\\~ Hello, world!"
                        |> parse
                        |> Expect.equal
                            [ Paragraph [ Text "~~Hi~~ Hello, world!" ] ]
            ]
        , describe "positional heuristic for inline vs block HTML"
            [ test "single-line HTML tag after paragraph text becomes inline" <|
                \() ->
                    "She speaks.\n<acerola>Hello!</acerola>"
                        |> parse
                        |> Expect.equal
                            [ Paragraph
                                [ Text "She speaks.\n"
                                , HtmlInline (HtmlElement "acerola" [] [ Text "Hello!" ] "Hello!")
                                ]
                            ]
            , test "single-line HTML followed by text on next line merges into paragraph" <|
                \() ->
                    "<foo>bar</foo>\nbaz"
                        |> parse
                        |> Expect.equal
                            [ Paragraph
                                [ HtmlInline (HtmlElement "foo" [] [ Text "bar" ] "bar")
                                , Text "\nbaz"
                                ]
                            ]
            , test "single-line HTML with trailing spaces followed by text merges into paragraph" <|
                \() ->
                    "<foo>bar</foo>  \nbaz"
                        |> parse
                        |> Expect.equal
                            [ Paragraph
                                [ HtmlInline (HtmlElement "foo" [] [ Text "bar" ] "bar")
                                , HardLineBreak
                                , Text "baz"
                                ]
                            ]
            , test "multi-line HTML always becomes block even interrupting paragraph" <|
                \() ->
                    "She speaks.\n<foo>\nHello!\n</foo>"
                        |> parse
                        |> Expect.equal
                            [ Paragraph [ Text "She speaks." ]
                            , HtmlBlock
                                (HtmlElement "foo"
                                    []
                                    [ Paragraph [ Text "Hello!" ] ]
                                    "\nHello!\n"
                                )
                            ]
            , test "blank line before single-line HTML makes it a block" <|
                \() ->
                    "<foo>bar</foo>\n\nbaz"
                        |> parse
                        |> Expect.equal
                            [ HtmlBlock (HtmlElement "foo" [] [ Paragraph [ Text "bar" ] ] "bar")
                            , Paragraph [ Text "baz" ]
                            ]
            , test "multi-line HTML followed by text stays as separate blocks" <|
                \() ->
                    "<foo>\nbar\n</foo>\nbaz"
                        |> parse
                        |> Expect.equal
                            [ HtmlBlock
                                (HtmlElement "foo"
                                    []
                                    [ Paragraph [ Text "bar" ] ]
                                    "\nbar\n"
                                )
                            , Paragraph [ Text "baz" ]
                            ]
            , test "single-line self-closing tag followed by text merges into paragraph" <|
                \() ->
                    "<my-widget />\nsome text"
                        |> parse
                        |> Expect.equal
                            [ Paragraph
                                [ HtmlInline (HtmlElement "my-widget" [] [] "")
                                , Text "\nsome text"
                                ]
                            ]
            , test "single-line comment followed by text merges into paragraph" <|
                \() ->
                    "<!-- hello -->\nsome text"
                        |> parse
                        |> Expect.equal
                            [ Paragraph
                                [ HtmlInline (HtmlComment " hello ")
                                , Text "\nsome text"
                                ]
                            ]
            , test "mid-line multi-line HTML does not parse as single inline element" <|
                \() ->
                    "This is my foo thing <foo>text\nmore text</foo>"
                        |> parse
                        |> Expect.equal
                            [ Paragraph
                                [ Text "This is my foo thing <foo>text\nmore text"
                                , HtmlInline (HtmlElement "/foo" [] [] "")
                                ]
                            ]
            ,test "mid-line single-line HTML still works inline" <|
                \() ->
                    "This is my foo thing <foo>text</foo> more"
                        |> parse
                        |> Expect.equal
                            [ Paragraph
                                [ Text "This is my foo thing "
                                , HtmlInline (HtmlElement "foo" [] [ Text "text" ] "text")
                                , Text " more"
                                ]
                            ]
            ]
        ]


plainListItem : String -> Block.ListItem Block.Block
plainListItem body =
    Block.ListItem Block.NoTask [ Block.Paragraph [ Block.Text body ] ]


unstyledText : String -> List Inline
unstyledText body =
    [ Block.Text body ]


emphasisText : String -> List Inline
emphasisText body =
    [ Block.Emphasis <|
        [ Block.Text body ]
    ]
