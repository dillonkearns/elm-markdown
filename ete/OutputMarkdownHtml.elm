port module OutputMarkdownHtml exposing (main)

import Html.String as Html
import Html.String.Attributes as Attr
import Markdown.Parser as Markdown


port printOutput : String -> Cmd msg


port error : String -> Cmd msg


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


renderMarkdown : String -> Html
renderMarkdown markdown =
    markdown
        |> Markdown.render
            { h1 = \content -> Html.h1 [] [ Html.text content ]
            , h2 = \content -> Html.h2 [] [ Html.text content ]
            , raw = Html.text
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
