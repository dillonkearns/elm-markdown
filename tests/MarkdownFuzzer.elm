module MarkdownFuzzer exposing (all, blockQuoteFuzzer, codeBlockFuzzer, decoratedWordFuzzer, divDefaultStringRenderer, headingFuzzer, htmlFuzzer, imageFuzzer, inlineFuzzer, linkFuzzer, list, orderedListFuzzer, paragraphFuzzer, plainWordFuzzer, randomMarkdown, tableFuzzer, thematicBreakFuzzer, unorderedListFuzzer, urlFuzzer)

import Expect
import Fuzz
import Markdown.Block exposing (Block)
import Markdown.Html
import Markdown.Parser
import Markdown.Renderer exposing (Renderer, defaultStringRenderer)
import Parser
import Parser.Advanced as Advanced
import Test exposing (fuzz)


divDefaultStringRenderer : Renderer String
divDefaultStringRenderer =
    { defaultStringRenderer | html = Markdown.Html.oneOf [ Markdown.Html.tag "div" (String.join "") ] }


all : Test.Test
all =
    fuzz randomMarkdown
        "testOutputs"
        (\randomMarkdownValue ->
            let
                randomMd : Result (List (Advanced.DeadEnd String Parser.Problem)) (List Block)
                randomMd =
                    randomMarkdownValue
                        |> Markdown.Parser.parse

                renderedStr : Result String (List String)
                renderedStr =
                    randomMd
                        |> Result.mapError (\e -> "markdown parse error " ++ Debug.toString e)
                        |> Result.andThen (\mkd -> Markdown.Renderer.render divDefaultStringRenderer mkd)

                -- parse again.
                parsed : Result String (List Block)
                parsed =
                    renderedStr
                        |> Result.andThen
                            (\strs ->
                                Markdown.Parser.parse (String.join "" strs)
                                    |> Result.mapError (\e -> List.map Markdown.Parser.deadEndToString e |> String.join "")
                            )
            in
            case parsed of
                Ok _ ->
                    Expect.pass

                Err e ->
                    Expect.fail e
        )


randomMarkdown : Fuzz.Fuzzer String
randomMarkdown =
    list
        (Fuzz.oneOf
            [ headingFuzzer
            , paragraphFuzzer
            , blockQuoteFuzzer
            , orderedListFuzzer
            , unorderedListFuzzer
            , htmlFuzzer
            , tableFuzzer
            , codeBlockFuzzer
            , thematicBreakFuzzer
            ]
        )
        |> Fuzz.map (String.join "\n\n")


linkFuzzer : Fuzz.Fuzzer String
linkFuzzer =
    Fuzz.map2
        (\label url ->
            "[" ++ label ++ "](" ++ url ++ ")"
        )
        plainWordFuzzer
        urlFuzzer


imageFuzzer : Fuzz.Fuzzer String
imageFuzzer =
    Fuzz.map2
        (\altText url ->
            "![" ++ altText ++ "](" ++ url ++ ")"
        )
        plainWordFuzzer
        urlFuzzer


urlFuzzer : Fuzz.Fuzzer String
urlFuzzer =
    plainWordFuzzer |> Fuzz.map (\s -> "http://" ++ s)


orderedListFuzzer : Fuzz.Fuzzer String
orderedListFuzzer =
    list
        inlineFuzzer
        |> Fuzz.map
            (\items ->
                items
                    |> List.indexedMap
                        (\i text ->
                            String.fromInt (i + 1) ++ ". " ++ text
                        )
                    |> String.join "\n"
            )


unorderedListFuzzer : Fuzz.Fuzzer String
unorderedListFuzzer =
    list
        inlineFuzzer
        |> Fuzz.map
            (\items ->
                items
                    |> List.map
                        (\text ->
                            "- " ++ text
                        )
                    |> String.join "\n"
            )


htmlFuzzer : Fuzz.Fuzzer String
htmlFuzzer =
    Fuzz.map
        (\s -> "<div>" ++ s ++ "</div>")
        inlineFuzzer


tableFuzzer : Fuzz.Fuzzer String
tableFuzzer =
    list
        (list inlineFuzzer)
        |> Fuzz.map
            (\rows ->
                rows
                    |> List.map (String.join " | ")
                    |> String.join "\n"
            )


codeBlockFuzzer : Fuzz.Fuzzer String
codeBlockFuzzer =
    Fuzz.map
        (\s -> "```\n" ++ s ++ "\n```")
        inlineFuzzer


thematicBreakFuzzer : Fuzz.Fuzzer String
thematicBreakFuzzer =
    Fuzz.constant "---"


paragraphFuzzer : Fuzz.Fuzzer String
paragraphFuzzer =
    list
        (Fuzz.pair Fuzz.bool plainWordFuzzer)
        |> Fuzz.map
            (\pairs ->
                pairs
                    |> List.foldl
                        (\( isNewline, word ) acc ->
                            if isNewline then
                                acc ++ "\n" ++ word

                            else
                                acc ++ " " ++ word
                        )
                        ""
            )


blockQuoteFuzzer : Fuzz.Fuzzer String
blockQuoteFuzzer =
    Fuzz.oneOf
        [ headingFuzzer
        , inlineFuzzer
        ]
        |> Fuzz.map
            (\inlineText ->
                "> " ++ inlineText
            )


headingFuzzer : Fuzz.Fuzzer String
headingFuzzer =
    Fuzz.map3
        (\inlineText level closingHeadingStyle ->
            let
                headingMark : String
                headingMark =
                    String.repeat level "#"
            in
            headingMark
                ++ " "
                ++ inlineText
                ++ (if closingHeadingStyle then
                        " " ++ headingMark

                    else
                        ""
                   )
        )
        inlineFuzzer
        (Fuzz.intRange 1 6)
        Fuzz.bool


inlineFuzzer : Fuzz.Fuzzer String
inlineFuzzer =
    list
        (Fuzz.oneOf
            [ plainWordFuzzer
            , decoratedWordFuzzer
            , linkFuzzer
            , imageFuzzer
            ]
        )
        |> Fuzz.map (String.join " ")


decoratedWordFuzzer : Fuzz.Fuzzer String
decoratedWordFuzzer =
    Fuzz.map4
        (\isEmphasized isStrong isStrikethrough plainWord ->
            plainWord
                |> (\value ->
                        if isEmphasized then
                            "*" ++ value ++ "*"

                        else
                            value
                   )
                |> (\value ->
                        if isStrong then
                            "**" ++ value ++ "**"

                        else
                            value
                   )
                |> (\value ->
                        if isStrikethrough then
                            "~~" ++ value ++ "~~"

                        else
                            value
                   )
        )
        Fuzz.bool
        Fuzz.bool
        Fuzz.bool
        plainWordFuzzer


plainWordFuzzer : Fuzz.Fuzzer String
plainWordFuzzer =
    list
        (Fuzz.oneOf
            [ Fuzz.intRange (Char.toCode 'A') (Char.toCode 'Z')
                |> Fuzz.map Char.fromCode
            , Fuzz.intRange (Char.toCode 'a') (Char.toCode 'z')
                |> Fuzz.map Char.fromCode
            ]
        )
        |> Fuzz.map String.fromList


list : Fuzz.Fuzzer a -> Fuzz.Fuzzer (List a)
list =
    Fuzz.listOfLengthBetween 0 10
