module CustomHtmlBlockSignupForm exposing (main)

import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Font as Font
import Element.Region
import Html exposing (Attribute, Html)
import Html.Attributes
import Markdown.Block exposing (Block, Inline, InlineStyle)
import Markdown.Html
import Markdown.Parser


main : Html msg
main =
    Element.layout []
        (case view markdownBody of
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
        )


markdownBody =
    """# Custom HTML Renderers

You just render it like this

```
<bio name="Dillon Kearns" photo="https://avatars2.githubusercontent.com/u/1384166">
Dillon really likes building things with Elm! Here are some links

- [Github](https://github.com/dillonkearns/)
- [Twitter](https://twitter.com/dillontkearns/)
- [Home page](https://incrementalelm.com/)
</bio>
```

And you get a custom view like this!

<bio name="Dillon Kearns" photo="https://avatars2.githubusercontent.com/u/1384166">
Dillon really likes building things with Elm! Here are some links

- [Github](https://github.com/dillonkearns/)
- [Twitter](https://twitter.com/dillontkearns/)
- [Home page](https://incrementalelm.com/)
</bio>
"""


buildToc : List Block -> TableOfContents
buildToc blocks =
    let
        headings =
            gatherHeadings blocks
    in
    headings
        |> List.map Tuple.second
        |> List.map
            (\styledList ->
                { anchorId = styledToString styledList |> rawTextToId
                , name = styledToString styledList
                , level = 1
                }
            )


styledToString : List Inline -> String
styledToString list =
    List.map .string list
        |> String.join "-"


gatherHeadings : List Block -> List ( Int, List Inline )
gatherHeadings blocks =
    List.filterMap
        (\block ->
            case block of
                Markdown.Block.Heading level content ->
                    Just ( level, content )

                _ ->
                    Nothing
        )
        blocks


type alias TableOfContents =
    List { anchorId : String, name : String, level : Int }


view : String -> Result String (List (Element msg))
view markdown =
    markdown
        |> Markdown.Parser.parse
        |> Result.mapError (\error -> error |> List.map Markdown.Parser.deadEndToString |> String.join "\n")
        |> Result.andThen (Markdown.Parser.render renderer)


renderer : Markdown.Parser.Renderer (Element msg)
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
    , list =
        \items ->
            Element.column [ Element.spacing 15 ]
                (items
                    |> List.map
                        (\itemBlocks ->
                            Element.row [ Element.spacing 5 ]
                                [ Element.el
                                    [ Element.alignTop ]
                                    (Element.text "•")
                                , itemBlocks
                                ]
                        )
                )
    , codeBlock = codeBlock
    , html =
        Markdown.Html.oneOf
            [ Markdown.Html.tag "bio" (\name photoUrl renderedChildren -> bioView renderedChildren name photoUrl)
                |> Markdown.Html.withAttribute "name"
                |> Markdown.Html.withAttribute "photo"
            ]
    }


bioView renderedChildren name photoUrl =
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
        (avatarView photoUrl
            :: Element.el
                [ Font.bold
                , Font.size 30
                ]
                (Element.text name)
            :: renderedChildren
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
