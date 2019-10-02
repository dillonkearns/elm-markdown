module ParagraphTests exposing (suite)

import Expect exposing (Expectation)
import Markdown.Block as Block exposing (Block)
import Markdown.InlineBlock as InlineBlock
import Markdown.Inlines
import Markdown.ParserNew exposing (..)
import Parser
import Parser.Advanced as Advanced
import Test exposing (..)


type alias Parser a =
    Advanced.Parser String Parser.Problem a


parse : String -> Result (List (Advanced.DeadEnd String Parser.Problem)) (List Block)
parse =
    Markdown.ParserNew.parse


suite : Test
suite =
    only <|
        describe "paragraphs"
            [ test "plain text on multiple lines is in one paragraph" <|
                \() ->
                    """Line 1
Line 2

Line after blank line"""
                        |> parse
                        |> Expect.equal (Ok [ Block.Body (unstyledText """Line 1
Line 2

Line after blank line""") ])
            ]


unstyledText : String -> List InlineBlock.StyledString
unstyledText body =
    [ { string = body
      , style =
            { isCode = False
            , isBold = False
            , isItalic = False
            , link = Nothing
            }
      }
    ]


unstyledTextSingle : String -> InlineBlock.StyledString
unstyledTextSingle body =
    { string = body
    , style =
        { isCode = False
        , isBold = False
        , isItalic = False
        , link = Nothing
        }
    }


parserError : String -> Expect.Expectation
parserError markdown =
    case parse markdown of
        Ok _ ->
            Expect.fail "Expected a parser failure!"

        Err _ ->
            Expect.pass
