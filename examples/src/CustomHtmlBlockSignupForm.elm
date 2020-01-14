module CustomHtmlBlockSignupForm exposing (main)

import Browser
import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Font as Font
import Element.Input
import Element.Region
import Html exposing (Attribute, Html)
import Html.Attributes
import Markdown.Block exposing (Block, Inline, InlineStyle)
import Markdown.Html
import Markdown.Parser


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
        |> Result.andThen (Markdown.Parser.render renderer)


renderer : Markdown.Parser.Renderer (Element Msg)
renderer =
    { heading = heading
    , raw =
        Element.paragraph
            [ Element.spacing 15 ]
    , thematicBreak = Element.none
    , plain = Element.text
    , bold = \content -> Element.row [ Font.bold ] [ Element.text content ]
    , italic = \content -> Element.row [ Font.italic ] [ Element.text content ]
    , code = code
    , link =
        \{ title, destination } body ->
            Element.newTabLink
                [ Element.htmlAttribute (Html.Attributes.style "display" "inline-flex") ]
                { url = destination
                , label =
                    Element.paragraph
                        [ Font.color (Element.rgb255 0 0 255)
                        ]
                        body
                }
                |> Ok
    , image =
        \image body ->
            Element.image [ Element.width Element.fill ] { src = image.src, description = body }
                |> Ok
    , unorderedList =
        \items ->
            Element.column [ Element.spacing 15 ]
                (items
                    |> List.map
                        (\itemBlocks ->
                            Element.row [ Element.spacing 5 ]
                                [ Element.row
                                    [ Element.alignTop ]
                                    (Element.text "• " :: itemBlocks)
                                ]
                        )
                )
    , orderedList =
        \startingIndex items ->
            Element.column [ Element.spacing 15 ]
                (items
                    |> List.indexedMap
                        (\index itemBlocks ->
                            Element.row [ Element.spacing 5 ]
                                [ Element.row [ Element.alignTop ]
                                    (Element.text (String.fromInt index ++ " ") :: itemBlocks)
                                ]
                        )
                )
    , codeBlock = codeBlock
    , html =
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


rawTextToId rawText =
    rawText
        |> String.toLower
        |> String.replace " " ""


heading : { level : Int, rawText : String, children : List (Element msg) } -> Element msg
heading { level, rawText, children } =
    Element.paragraph
        [ Font.size
            (case level of
                1 ->
                    36

                2 ->
                    24

                _ ->
                    20
            )
        , Font.bold
        , Font.family [ Font.typeface "Montserrat" ]
        , Element.Region.heading level
        , Element.htmlAttribute
            (Html.Attributes.attribute "name" (rawTextToId rawText))
        , Font.center
        , Element.htmlAttribute
            (Html.Attributes.id (rawTextToId rawText))
        ]
        children


code : String -> Element msg
code snippet =
    Element.el
        [ Element.Background.color
            (Element.rgba 0 0 0 0.04)
        , Element.Border.rounded 2
        , Element.paddingXY 5 3
        , Font.family
            [ Font.external
                { url = "https://fonts.googleapis.com/css?family=Source+Code+Pro"
                , name = "Source Code Pro"
                }
            ]
        ]
        (Element.text snippet)


codeBlock : { body : String, language : Maybe String } -> Element msg
codeBlock details =
    Element.el
        [ Element.Background.color (Element.rgba 0 0 0 0.03)
        , Element.htmlAttribute (Html.Attributes.style "white-space" "pre")
        , Element.padding 20
        , Element.width Element.fill
        , Font.family
            [ Font.external
                { url = "https://fonts.googleapis.com/css?family=Source+Code+Pro"
                , name = "Source Code Pro"
                }
            ]
        ]
        (Element.text details.body)


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
