module Example exposing (..)

import Expect exposing (Expectation)
import Parser
import Parser.Advanced as Advanced exposing ((|.), (|=), Nestable(..), Step(..), andThen, chompUntil, chompWhile, getChompedString, inContext, int, lazy, loop, map, multiComment, oneOf, problem, succeed, symbol, token)
import Test exposing (..)
import XmlParser exposing (Node(..))


type alias Parser a =
    Advanced.Parser String Parser.Problem a


type Block
    = Heading Int String
    | Body String
    | Html HtmlNode


type alias Attribute =
    { name : String, value : String }


type HtmlNode
    = Element String (List Attribute) (List HtmlNode)
      -- | Text String
    | InnerBlocks (List Block)


body : Parser Block
body =
    succeed Body
        |= getChompedString
            (succeed ()
                |. chompWhile (\c -> c /= '\n')
            )


lineParser : Parser Block
lineParser =
    oneOf
        [ heading
        , htmlParser
        , body
        ]


htmlParser : Parser Block
htmlParser =
    XmlParser.element
        |> xmlNodeToHtmlNode
        |> Advanced.map Html


xmlNodeToHtmlNode : Parser Node -> Parser HtmlNode
xmlNodeToHtmlNode parser =
    Advanced.andThen
        (\xmlNode ->
            case xmlNode of
                XmlParser.Text innerText ->
                    -- InnerBlocks []
                    -- Advanced.andThen (\_ -> Debug.todo "")
                    -- Advanced.problem ""
                    -- anything
                    -- Advanced.succeed (InnerBlocks [])
                    InnerBlocks [ Body innerText ]
                        |> Advanced.succeed

                XmlParser.Element tag attributes children ->
                    Advanced.andThen
                        (\parsedChildren ->
                            Advanced.succeed
                                (Element tag
                                    attributes
                                    [ InnerBlocks parsedChildren ]
                                )
                        )
                        (thing children)
        )
        parser


thing : List Node -> Parser (List Block)
thing children =
    -- Advanced.succeed []
    children
        |> List.map childToParser
        |> combine


combine : List (Parser a) -> Parser (List a)
combine list =
    list
        |> List.foldl
            (\parser listParser ->
                listParser
                    |> Advanced.andThen
                        (\soFar ->
                            parser
                                |> Advanced.map (\a -> a :: soFar)
                        )
            )
            (Advanced.succeed [])


childToParser : Node -> Parser Block
childToParser node =
    (case node of
        XmlParser.Element tag attributes [] ->
            Advanced.succeed (Element tag attributes [])

        Text innerText ->
            InnerBlocks [ Body (innerText |> String.trim) ]
                |> Advanced.succeed

        _ ->
            Debug.todo "handle Element _ _ (_ :: _)"
    )
        |> Advanced.map Html


multiParser : Parser (List Block)
multiParser =
    loop [] statementsHelp


statementsHelp : List Block -> Parser (Step (List Block) (List Block))
statementsHelp revStmts =
    oneOf
        [ succeed (\stmt -> Loop (stmt :: revStmts))
            |= lineParser
            |. symbol (Advanced.Token "\n" (Parser.Expecting "newline"))
        , succeed ()
            |> map (\_ -> Done (List.reverse revStmts))
        ]


heading : Parser Block
heading =
    succeed Heading
        |. symbol (Advanced.Token "#" (Parser.Expecting "#"))
        |= (getChompedString
                (succeed ()
                    |. chompWhile (\c -> c == '#')
                )
                |> map
                    (\additionalHashes ->
                        String.length additionalHashes + 1
                    )
           )
        |. chompWhile (\c -> c == ' ')
        |= getChompedString (succeed () |. chompWhile (\c -> c /= '\n'))


parse : String -> Result (List (Advanced.DeadEnd String Parser.Problem)) Block
parse input =
    Advanced.run lineParser input


suite : Test
suite =
    describe "parsing"
        [ describe "headings"
            [ test "Heading 1" <|
                \() ->
                    "# Hello!"
                        |> parse
                        |> Expect.equal (Ok (Heading 1 "Hello!"))
            , test "Heading 2" <|
                \() ->
                    "## Hello!"
                        |> parse
                        |> Expect.equal (Ok (Heading 2 "Hello!"))
            , skip <|
                -- TODO limit parsing over heading level 7, see https://spec.commonmark.org/0.27/#atx-headings
                test "Heading 7 is invalid"
                <|
                    \() ->
                        "####### Hello!"
                            |> parserError
            ]
        , test "plain text" <|
            \() ->
                "This is just some text"
                    |> parse
                    |> Expect.equal (Ok (Body "This is just some text"))
        , test "parse heading then plain text" <|
            \() ->
                """# Heading
This is just some text
"""
                    |> Advanced.run multiParser
                    |> Expect.equal
                        (Ok
                            [ Heading 1 "Heading"
                            , Body "This is just some text"
                            ]
                        )
        , skip <|
            test "doesn't need to end in newline" <|
                \() ->
                    """# Heading
                        This is just some text
                        """
                        |> Advanced.run multiParser
                        |> Expect.equal
                            (Ok
                                [ Heading 1 "Heading"
                                , Body "This is just some text"
                                ]
                            )
        , test "long example" <|
            \() ->
                """# Heading

This is just some text.

## Subheading

Body of the subheading.
"""
                    |> Advanced.run multiParser
                    |> Expect.equal
                        (Ok
                            [ Heading 1 "Heading"
                            , Body ""
                            , Body "This is just some text."
                            , Body ""
                            , Heading 2 "Subheading"
                            , Body ""
                            , Body "Body of the subheading."
                            ]
                        )
        , test "embedded HTML" <|
            \() ->
                """# Heading
<div>
  Hello!
</div>
"""
                    |> Advanced.run multiParser
                    |> Expect.equal
                        (Ok
                            [ Heading 1 "Heading"
                            , Html
                                (Element "div"
                                    []
                                    [ InnerBlocks
                                        [ Body "Hello!"
                                        ]
                                    ]
                                )
                            ]
                        )
        ]


parserError : String -> Expect.Expectation
parserError markdown =
    case parse markdown of
        Ok _ ->
            Expect.fail "Expected a parser failure!"

        Err _ ->
            Expect.pass
