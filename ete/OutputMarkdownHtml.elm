port module OutputMarkdownHtml exposing (main)

import Html.String as Html
import Html.String.Attributes as Attr
import Markdown.Inlines exposing (StyledString)
import Markdown.Parser as Markdown


port printOutput : String -> Cmd msg


port error : String -> Cmd msg



--


printHtml : Html -> Cmd msg
printHtml renderResult =
    case renderResult of
        Ok htmlString ->
            printOutput htmlString

        Err errorString ->
            error errorString


type alias Html =
    Result String String


init markdown =
    ( ()
    , markdown
        |> renderMarkdown
        |> printHtml
    )


styledStringView : StyledString -> Html.Html msg
styledStringView { style, string } =
    if style.isBold then
        Html.strong [] [ Html.text string ]

    else if style.isItalic then
        Html.em [] [ Html.text string ]

    else if style.isCode then
        Html.code [] [ Html.text string ]

    else
        Html.text string


renderMarkdown : String -> Html
renderMarkdown markdown =
    markdown
        |> Markdown.render
            { heading =
                \level content ->
                    case level of
                        1 ->
                            Html.h1 [] content

                        2 ->
                            Html.h2 [] content

                        3 ->
                            Html.h3 [] content

                        4 ->
                            Html.h4 [] content

                        5 ->
                            Html.h5 [] content

                        6 ->
                            Html.h6 [] content

                        _ ->
                            Html.text "TODO maye use a type here to clean it up... this will never happen"
            , raw = Html.p []
            , bold =
                \content -> Html.strong [] [ Html.text content ]
            , italic =
                \content -> Html.em [] [ Html.text content ]
            , code =
                \content -> Html.code [] [ Html.text content ]
            , link =
                -- TODO use link.title
                \link content -> Html.a [ Attr.href link.destination ] [ Html.text content ]
            , plain =
                Html.text

            -- \styledStrings ->
            --     Html.p []
            --         (List.map styledStringView styledStrings)
            , todo = Html.text "TODO"
            , htmlDecoder = Markdown.htmlOneOf []
            }
        |> Result.map (List.map (Html.toString 0))
        |> Result.map (String.join "")


main =
    Platform.worker
        { init = init
        , update = \msg model -> ( model, Cmd.none )
        , subscriptions = \model -> Sub.none
        }
