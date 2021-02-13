module RenderingAFunction exposing (main)

import Browser
import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Font as Font
import Element.Input
import Element.Region
import Html exposing (Attribute, Html)
import Html.Attributes
import Markdown.Block as Block exposing (ListItem(..), Task(..))
import Markdown.Html
import Markdown.Parser
import Markdown.Renderer


view : Model -> { title : String, body : List (Html Msg) }
view model =
    { title = "dillonkearns/elm-markdown demo"
    , body =
        [ Element.layout [ Element.width Element.fill ]
            (Element.row
                [ Element.width Element.fill
                , Element.padding 50
                ]
                [ Element.Input.multiline [ Element.width (Element.px 400) ]
                    { onChange = OnMarkdownInput
                    , text = model.markdown
                    , placeholder = Nothing
                    , label = Element.Input.labelHidden "Markdown input"
                    , spellcheck = False
                    }
                , Element.column
                    [ Element.width Element.fill
                    ]
                    [ fontSizeButtons model.zoomFactor
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
                ]
            )
        ]
    }


fontSizeButtons : Int -> Element Msg
fontSizeButtons current =
    Element.column [ Element.spacing 20 ]
        [ Element.text "These buttons change the heading size. Try using the zoom factor for more font sizes as an exercise!"
        , Element.row
            [ Element.centerX
            , Element.width (Element.px 100)
            , Element.padding 20
            , Element.spacing 10
            ]
            [ fontSizeButton 1 current
            , fontSizeButton 2 current
            , fontSizeButton 3 current
            ]
        ]


fontSizeButton : Int -> Int -> Element Msg
fontSizeButton zoomFactor current =
    let
        baseOptions =
            [ Element.Border.width 3
            , Element.paddingEach { top = 10, bottom = 10, left = 40, right = 40 }
            ]

        options =
            if current == zoomFactor then
                Element.Border.color (Element.rgb255 150 0 150)
                    :: Font.bold
                    :: baseOptions

            else
                baseOptions
    in
    Element.Input.button
        options
        { label = Element.text <| String.fromInt zoomFactor ++ "x"
        , onPress = Just <| SetFontSize zoomFactor
        }


markdownView : Model -> Result String (List (Element Msg))
markdownView model =
    model.markdown
        |> Markdown.Parser.parse
        |> Result.mapError (\error -> error |> List.map Markdown.Parser.deadEndToString |> String.join "\n")
        |> Result.andThen (Markdown.Renderer.render (renderer model.zoomFactor))


renderer : Int -> Markdown.Renderer.Renderer (Element Msg)
renderer zoomFactor =
    { heading = heading zoomFactor
    , paragraph =
        \body ->
            Element.paragraph [ Element.spacing 15 ] body
    , thematicBreak = Element.none
    , text = \value -> Element.text value
    , strong = \content -> Element.row [ Font.bold ] content
    , emphasis = \content -> Element.row [ Font.italic ] content
    , strikethrough = \content -> Element.row [ Font.strike ] content
    , hardLineBreak = Element.row [ Element.height (Element.px 15) ] []
    , codeSpan = code
    , link = link
    , image =
        \image ->
            Element.image
                [ Element.width Element.fill ]
                { src = image.src, description = image.alt }
    , blockQuote =
        \children ->
            Element.paragraph
                [ Element.Border.widthEach { top = 0, right = 0, bottom = 0, left = 10 }
                , Element.padding 10
                , Element.Border.color (Element.rgb255 145 145 145)
                , Element.Background.color (Element.rgb255 245 245 245)
                ]
                children
    , unorderedList =
        \items ->
            Element.column [ Element.spacing 15 ]
                (items
                    |> List.map
                        (\(ListItem task children) ->
                            Element.row [ Element.spacing 5 ]
                                [ Element.row
                                    [ Element.alignTop ]
                                    ((case task of
                                        IncompleteTask ->
                                            Element.Input.defaultCheckbox False

                                        CompletedTask ->
                                            Element.Input.defaultCheckbox True

                                        NoTask ->
                                            Element.text "•"
                                     )
                                        :: Element.text " "
                                        :: children
                                    )
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
                                    (Element.text (String.fromInt (startingIndex + index) ++ " ")
                                        :: itemBlocks
                                    )
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
    , table = Element.column []
    , tableHeader = Element.column []
    , tableBody = Element.column []
    , tableRow = Element.row []
    , tableHeaderCell =
        \maybeAlignment children ->
            Element.paragraph [] children
    , tableCell =
        \maybeAlignment children ->
            Element.paragraph [] children
    }


link : { title : Maybe String, destination : String } -> List (Element Msg) -> Element Msg
link { title, destination } body =
    Element.newTabLink
        [ Element.htmlAttribute (Html.Attributes.style "display" "inline-flex") ]
        { url = destination
        , label =
            Element.paragraph
                [ Font.color (Element.rgb255 0 0 255)
                ]
                body
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


heading : Int -> { level : Block.HeadingLevel, rawText : String, children : List (Element msg) } -> Element msg
heading zoomFactor { level, rawText, children } =
    Element.paragraph
        [ Font.size
            (case level of
                Block.H1 ->
                    36 * zoomFactor

                Block.H2 ->
                    24 * zoomFactor

                _ ->
                    20 * zoomFactor
            )
        , Font.bold
        , Font.family [ Font.typeface "Montserrat" ]
        , Element.Region.heading (Block.headingLevelToInt level)
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
>
Dillon really likes building things with Elm! Here are some links

- [Articles](https://incrementalelm.com/articles)
</bio>

Note that these attributes are all optional. Try removing them and see what happens!
Or you can add `github="dillonkearns"` and see that icon show up. Or try making a `<bio>` tag
with your info!
"""


type Msg
    = OnMarkdownInput String
    | SetFontSize Int


type alias Flags =
    ()


type alias Model =
    { markdown : String
    , zoomFactor : Int
    }


main : Platform.Program Flags Model Msg
main =
    Browser.document
        { init =
            \flags ->
                ( { markdown = markdownBody
                  , zoomFactor = 2
                  }
                , Cmd.none
                )
        , view = view
        , update = update
        , subscriptions = \model -> Sub.none
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnMarkdownInput newMarkdown ->
            ( { model | markdown = newMarkdown }, Cmd.none )

        SetFontSize float ->
            ( { model | zoomFactor = float }, Cmd.none )
