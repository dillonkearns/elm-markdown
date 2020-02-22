module CustomHtmlBlockSignupForm exposing (main)

import Browser
import Element exposing (Element)
import Element.Border
import Element.Font as Font
import Element.Input
import ElmUi
import Html exposing (Attribute, Html)
import Markdown.Html
import Markdown.Parser
import Markdown.Renderer


view : Model -> { title : String, body : List (Html Msg) }
view model =
    { title = "dillonkearns/elm-markdown demo"
    , body =
        [ Element.layout [ Element.width Element.fill ]
            (Element.row [ Element.width Element.fill ]
                [ Element.Input.multiline [ Element.width (Element.px 400) ]
                    { onChange = OnMarkdownInput
                    , text = model
                    , placeholder = Nothing
                    , label = Element.Input.labelHidden "Markdown input"
                    , spellcheck = False
                    }
                , case markdownView model of
                    Ok rendered ->
                        Element.column
                            [ Element.spacing 30
                            , Element.padding 80
                            , Element.width (Element.fill |> Element.maximum 1000)
                            , Element.centerX
                            ]
                            rendered

                    Err errors ->
                        Element.text errors
                ]
            )
        ]
    }


markdownView : String -> Result String (List (Element Msg))
markdownView markdown =
    markdown
        |> Markdown.Parser.parse
        |> Result.mapError (\error -> error |> List.map Markdown.Parser.deadEndToString |> String.join "\n")
        |> Result.andThen (Markdown.Renderer.render renderer)


elmUiRenderer =
    ElmUi.renderer


renderer : Markdown.Renderer.Renderer (Element Msg)
renderer =
    { elmUiRenderer
        | html =
            Markdown.Html.oneOf
                [ Markdown.Html.tag "bio"
                    (\name photoUrl twitter github dribbble renderedChildren ->
                        bioView renderedChildren name photoUrl twitter github dribbble
                    )
                    |> Markdown.Html.withAttribute "name"
                    |> Markdown.Html.withAttribute "photo"
                    |> Markdown.Html.withOptionalAttribute "twitter"
                    |> Markdown.Html.withOptionalAttribute "github"
                    |> Markdown.Html.withOptionalAttribute "dribbble"
                ]
    }


bioView renderedChildren name photoUrl twitter github dribbble =
    Element.column
        [ Element.Border.shadow
            { offset = ( 0.3, 0.3 )
            , size = 2
            , blur = 0.5
            , color = Element.rgba255 0 0 0 0.22
            }
        , Element.padding 20
        , Element.spacing 30
        , Element.centerX
        , Font.center
        ]
        (Element.row [ Element.spacing 20 ]
            [ avatarView photoUrl
            , Element.el
                [ Font.bold
                , Font.size 30
                ]
                (Element.text name)
            , icons twitter github dribbble
            ]
            :: renderedChildren
        )


icons twitter github dribbble =
    Element.row []
        ([ twitter
            |> Maybe.map
                (\twitterHandle ->
                    Element.link []
                        { url = "https://twitter.com/" ++ twitterHandle
                        , label =
                            Element.image [] { src = "https://i.imgur.com/tXSoThF.png", description = "Twitter Logo" }
                        }
                )
         , github
            |> Maybe.map
                (\twitterHandle ->
                    Element.link []
                        { url = "https://github.com/" ++ twitterHandle
                        , label =
                            Element.image [] { src = "https://i.imgur.com/0o48UoR.png", description = "Github Logo" }
                        }
                )
         , dribbble
            |> Maybe.map
                (\dribbbleHandle ->
                    Element.link []
                        { url = "https://dribbble.com/" ++ dribbbleHandle
                        , label =
                            Element.image [] { src = "https://i.imgur.com/1AGmwO3.png", description = "Dribbble Logo" }
                        }
                )
         ]
            |> List.filterMap identity
        )


avatarView avatarUrl =
    Element.image [ Element.width Element.fill ]
        { src = avatarUrl, description = "Avatar image" }
        |> Element.el
            [ Element.width (Element.px 80) ]


markdownBody =
    """# Custom HTML Renderers

You just render it like this

```
<bio
  name="Dillon Kearns"
  photo="https://avatars2.githubusercontent.com/u/1384166"
  twitter="dillontkearns"
  github="dillonkearns"
>
Dillon really likes building things with Elm! Here are some links

- [Articles](https://incrementalelm.com/articles)
</bio>
```

And you get a custom view like this!

<bio
  name="Dillon Kearns"
  photo="https://avatars2.githubusercontent.com/u/1384166"
  twitter="dillontkearns"
  github="dillonkearns"
>
Dillon really likes building things with Elm! Here are some links

- [Articles](https://incrementalelm.com/articles)
</bio>

Note that these attributes are all optional. Try removing them and see what happens!
Or you can add `dribbble="something"` and see that icon show up if it's provided.
"""


type Msg
    = OnMarkdownInput String


type alias Flags =
    ()


type alias Model =
    String


main : Platform.Program Flags Model Msg
main =
    Browser.document
        { init = \flags -> ( markdownBody, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \model -> Sub.none
        }


update msg model =
    case msg of
        OnMarkdownInput newMarkdown ->
            ( newMarkdown, Cmd.none )
