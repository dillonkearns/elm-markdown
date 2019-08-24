port module OutputMarkdownHtml exposing (main)

import Html.String as Html
import Html.String.Attributes as Attr
import Markdown.Inlines exposing (StyledString)
import Markdown.Parser as Markdown


port printOutput : String -> Cmd msg


port error : String -> Cmd msg



-- render : Result a (List StyledString) -> Html msg
-- render result =
--   case result of
--     Ok styledString ->
--       List.map styledStringView styledString |> Html.div []
--     Err errors ->
--       Html.text "Errors"
--


styledStringView : StyledString -> Html.Html msg
styledStringView { style, string } =
    -- [ Html.text string ]
    if style.isBold then
        Html.em [] [ Html.text string ]

    else
        Html.text string



-- |> Html.span
--     ([ Attr.style "font-weight" "bold"
--         |> (if style.isBold then
--                 Just
--
--             else
--                 \_ -> Nothing
--            )
--      , Attr.style "font-style" "italic"
--         |> (if style.isItalic then
--                 Just
--
--             else
--                 \_ -> Nothing
--            )
--      ]
--         |> List.filterMap identity
--     )


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
            , raw =
                \styledStrings ->
                    Html.p []
                        (List.map styledStringView styledStrings)
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
