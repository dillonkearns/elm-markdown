port module OutputMarkdownHtml exposing (main)


port printOutput : String -> Cmd msg


printHtml : Html -> Cmd msg
printHtml html =
    printOutput html


type alias Html =
    String


init markdown =
    ( ()
    , markdown
        |> renderMarkdown
        |> printHtml
    )


renderMarkdown : String -> Html
renderMarkdown markdown =
    "<hr />"


main =
    Platform.worker
        { init = init
        , update = \msg model -> ( model, Cmd.none )
        , subscriptions = \model -> Sub.none
        }
