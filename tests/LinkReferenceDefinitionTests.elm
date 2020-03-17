module LinkReferenceDefinitionTests exposing (suite)

import Expect exposing (Expectation)
import Markdown.LinkReferenceDefinition as LinkReferenceDefinition
import Parser.Advanced as Advanced
import Test exposing (..)


suite : Test
suite =
    describe "link reference definitions"
        [ test "basic definition with title" <|
            \() ->
                """[foo]: /url "title"
"""
                    |> expectParseTo
                        ( "foo", { destination = "/url", title = Just "title" } )
        , test "single-quoted title with link in angle brackets" <|
            \() ->
                """[Foo bar]:
<my url>
'title'
"""
                    |> expectParseTo
                        ( "foo bar", { destination = "my%20url", title = Just "title" } )
        , test "title is optional" <|
            \() ->
                "[foo]: /url"
                    |> expectParseTo
                        ( "foo", { destination = "/url", title = Nothing } )
        , test "title is optional ending with whitespace" <|
            \() ->
                "[foo]: /url\n"
                    |> expectParseTo
                        ( "foo", { destination = "/url", title = Nothing } )
        , test "does not succeed with missing destination" <|
            \() ->
                "[foo]:\n"
                    |> expectError
        , test "labels are stored in all lowercase in order to do case-insensitive matches" <|
            \() ->
                """[Foo bar]: <my url> 'title'"""
                    |> expectParseTo
                        ( "foo bar", { destination = "my%20url", title = Just "title" } )
        ]


expectParseTo :
    LinkReferenceDefinition.LinkReferenceDefinition
    -> String
    -> Expect.Expectation
expectParseTo expected input =
    input
        |> Advanced.run LinkReferenceDefinition.parser
        |> Expect.equal (Ok expected)


expectError : String -> Expectation
expectError input =
    case input |> Advanced.run LinkReferenceDefinition.parser of
        Ok value ->
            Expect.fail <| "Expecting an error. Got:\n\n" ++ Debug.toString value

        Err _ ->
            Expect.pass
