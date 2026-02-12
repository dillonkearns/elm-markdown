module Markdown.HtmlRenderer exposing (Attribute, HtmlRenderer(..), htmlElementToString)

import Markdown.Block as Block exposing (Block)


type alias Attribute =
    { name : String, value : String }


type HtmlRenderer a
    = HtmlRenderer (String -> List Attribute -> List Block -> Result String a)


htmlElementToString : String -> List Attribute -> List Block -> String
htmlElementToString tagName attributes children =
    let
        openTag : String
        openTag =
            escapeHtml (tagToString tagName attributes)

        childrenText : String
        childrenText =
            Block.foldl
                (\block soFar ->
                    case block of
                        Block.Paragraph inlines ->
                            soFar ++ Block.extractInlineText inlines

                        Block.Heading _ inlines ->
                            soFar ++ Block.extractInlineText inlines

                        Block.CodeBlock { body } ->
                            soFar ++ body

                        _ ->
                            soFar
                )
                ""
                children
    in
    if List.isEmpty children && String.isEmpty childrenText then
        openTag

    else
        openTag ++ childrenText ++ escapeHtml ("</" ++ tagName ++ ">")


tagToString : String -> List Attribute -> String
tagToString tagName attributes =
    if List.isEmpty attributes then
        "<" ++ tagName ++ ">"

    else
        "<" ++ tagName ++ " " ++ attributesToString attributes ++ ">"


attributesToString : List Attribute -> String
attributesToString attributes =
    attributes
        |> List.map
            (\{ name, value } ->
                name ++ "=\"" ++ value ++ "\""
            )
        |> String.join " "


escapeHtml : String -> String
escapeHtml raw =
    raw
        |> String.replace "&" "&amp;"
        |> String.replace "<" "&lt;"
        |> String.replace ">" "&gt;"
        |> String.replace "\"" "&quot;"
