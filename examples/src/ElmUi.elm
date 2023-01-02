module ElmUi exposing (main, renderer)

import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Font as Font
import Element.Input
import Element.Region
import Html exposing (Attribute, Html)
import Html.Attributes
import Markdown.Block as Block exposing (Block, Inline, ListItem(..), Task(..))
import Markdown.Html
import Markdown.Parser
import Markdown.Renderer


main : Html msg
main =
    Element.layout []
        (case view markdownBody of
            Ok rendered ->
                Element.column
                    [ Element.spacing 30
                    , Element.padding 50
                    ]
                    rendered

            Err errors ->
                Element.text errors
        )


markdownBody =
    """
# Esse intrata referre inter adspeximus aequora soror

## Todos
- [ ] A
- [ ] B
- [X] C

## Nested Lists

- Item 1
  - Nested item
  - Sibling
    - Another level of nesting
  - Sibling 2

## Block quotes

> This is a
> block quote

## Ebur iamque mecum

Lorem markdownum [vitae](http://minantia.io/herbis-caelumque.aspx) crines
carminibus exponit pugnax dilectaque Sparte te est. Nullos imperium! Ait qui
corpora perstat Gryneus fidem iunctura. Hic sperne inquit iuventus timidasque
iuvenis stirpe barbarus sorori? Fatebor non in iaculatur, concuteret auget
corpore accepere vectus pacisci quoque renascitur essem: frugum labentibus
Naxoque festum despectat.

## Saepe nec tergo Iole te solent pharetras

Iamque insula, ore longe dixerat libratum neque terrarum resedit de iuranda cum
muneris *tamen*, suas populique te. Alumno invidiae cecinit exarsit modo vidit
ingentia suum, et pluribus sensu *Danais* adigitque acervo gravis visae,
capillos!

```
var post = client_script_agp(bar_address_golden(flash, digital_halftone_unix
        + 3), supercomputer_bridge);
type_ctr_drag(waveform, -2, core_rom);
if (frameworkFile * wiki_ddr >= architecture_core(station_mebibyte, 5,
        dual_cd)) {
    tft.logMirror(infotainmentPram + spoofingArchitectureServer,
            sliRiscParse + mmsClean);
}
```

## Vetustas caede

Grata raucaque dixit delenda terris. Actorum circumdata fronde fuerat, accepisse
certe, haurit manu. [Ventura Achille](http://www.devolenti.net/canoro.php)
admovit, non ut tempto violas est ego pater; fit probavit iaculi Ophiusiaque
inque. Conlectae est premebat subsunt. Dum ad adusque sol sub vini, quod: per
sanguine, recludit posuisti: Trinacris Sibyllae.

## Tamquam novus

Nec munera pia sequuntur consedit est vultus, **enim laeva**, hortaturque
sulphura fraterna somni [circumstantes](http://illis-vacant.io/susmater.php)?
Futurae habet visa cogit natus coeperat lacertos luxuriem, coloribus quaecumque
unus membra? Et molirique saevior terrae concubiturus pars. Aequor convivia ergo
nec salutem, absentem veris exspirat, traxit deiectoque dedignata evolat pressit
me promissa amor ardor.

Iuvenem fluunt populusque iterum arcet donavi testatos tellus semperque
debueram. Edidit [illo votis](http://sum.net/cumfelixque) Melicerta *vivus*,
mare, **praefoderat iubasque**? Dabat **temerarius boves orbe** populi!

## Sic colitur tecum exsultantemque fessis vidit rescindere

Polydoreo Iovis mentis fratre posse, claudit placabilis nisi radiante premunt,
cum committitur, inquit bovem caput, vocem! De sensit vestigia super. Effugit
nux tamen nota pererrat nec semel erat: quater e solvi non nec **inmitem
tristi**.

Talia litore glomerataque quantum lentaque **restat**, nec lapsa Threiciis
subiere tamen exercere et tuis. Est fine cum supposito iamque, ex templa illa
cursus venerit tenebat et? Quemque mihi, dare erudit Lyncus, ab dicebar iterum
exanimi sermone; esse Iunonem paelicis mundi velit.

*Tydiden dubitabile neque* conscendere ardor verboque sic refert Auroram
sequantur praemia doleam pectusque fumantia hospes, cum silvaque caputque. Domat
et annis corpus est aperire amoris. Concha non quae columbas, quae tenuem,
pervia, euntis?

## Here is a table

| foo | bar |
| --- | --- |
| baz | bim |
| abc | def |
| longer cell here | shorter |
| and this is a very very very very very very very long cell | and this is a very very very very very very very long cell, too |

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
                { anchorId = styledList |> inlinesToId
                , name = styledToString styledList
                , level = 1
                }
            )


styledToString : List Inline -> String
styledToString inlines =
    --List.map .string list
    --|> String.join "-"
    -- TODO do I need to hyphenate?
    inlines
        |> Block.extractInlineText


inlinesToId : List Inline -> String
inlinesToId list =
    list
        |> Block.extractInlineText
        |> Debug.log "extracted"
        |> String.split " "
        |> Debug.log "split"
        |> String.join "-"
        |> Debug.log "joined"


gatherHeadings : List Block -> List ( Block.HeadingLevel, List Inline )
gatherHeadings blocks =
    List.filterMap
        (\block ->
            case block of
                Block.Heading level content ->
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
        |> Result.andThen (Markdown.Renderer.render renderer)


renderer : Markdown.Renderer.Renderer (Element msg)
renderer =
    { heading = heading
    , paragraph =
        Element.paragraph
            [ Element.spacing 15 ]
    , thematicBreak = Element.none
    , text = \value -> Element.paragraph [] [ Element.text value ]
    , strong = \content -> Element.paragraph [ Font.bold ] content
    , emphasis = \content -> Element.paragraph [ Font.italic ] content
    , strikethrough = \content -> Element.paragraph [ Font.strike ] content
    , codeSpan = code
    , link =
        \{ title, destination } body ->
            Element.newTabLink []
                { url = destination
                , label =
                    Element.paragraph
                        [ Font.color (Element.rgb255 0 0 255)
                        , Element.htmlAttribute (Html.Attributes.style "overflow-wrap" "break-word")
                        , Element.htmlAttribute (Html.Attributes.style "word-break" "break-word")
                        ]
                        body
                }
    , hardLineBreak = Html.br [] [] |> Element.html
    , image =
        \image ->
            case image.title of
                Just title ->
                    Element.image [ Element.width Element.fill ] { src = image.src, description = image.alt }

                Nothing ->
                    Element.image [ Element.width Element.fill ] { src = image.src, description = image.alt }
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
            Element.column [ Element.paddingXY 10 0 ]
                (items
                    |> List.map
                        (\(ListItem task children) ->
                            Element.paragraph []
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
                            Element.paragraph [ Element.spacing 5 ]
                                [ Element.paragraph [ Element.alignTop ]
                                    (Element.text (String.fromInt (index + startingIndex) ++ " ") :: itemBlocks)
                                ]
                        )
                )
    , codeBlock = codeBlock
    , table = Element.column []
    , tableHeader =
        Element.column
            [ Font.bold
            , Element.width Element.fill
            , Font.center
            ]
    , tableBody = Element.column []
    , tableRow = Element.row [ Element.height Element.fill, Element.width Element.fill ]
    , tableHeaderCell =
        \maybeAlignment children ->
            Element.paragraph
                tableBorder
                children
    , tableCell =
        \maybeAlignment children ->
            Element.paragraph
                tableBorder
                children
    , html = Markdown.Html.oneOf []
    }


alternateTableRowBackground =
    Element.rgb255 245 247 249


tableBorder =
    [ Element.Border.color (Element.rgb255 223 226 229)
    , Element.Border.width 1
    , Element.Border.solid
    , Element.paddingXY 6 13
    , Element.height Element.fill
    ]


rawTextToId : String -> String
rawTextToId rawText =
    rawText
        |> String.split " "
        |> String.join "-"
        |> String.toLower


heading : { level : Block.HeadingLevel, rawText : String, children : List (Element msg) } -> Element msg
heading { level, rawText, children } =
    Element.paragraph
        [ Font.size
            (case level of
                Block.H1 ->
                    36

                Block.H2 ->
                    24

                _ ->
                    20
            )
        , Font.bold
        , Font.family [ Font.typeface "Montserrat" ]
        , Element.Region.heading (Block.headingLevelToInt level)
        , Element.htmlAttribute
            (Html.Attributes.attribute "name" (rawTextToId rawText))
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
    Element.paragraph
        [ Element.Background.color (Element.rgba 0 0 0 0.03)
        , Element.htmlAttribute (Html.Attributes.style "white-space" "pre")
        , Element.htmlAttribute (Html.Attributes.style "overflow-wrap" "break-word")
        , Element.htmlAttribute (Html.Attributes.style "word-break" "break-word")
        , Element.padding 20
        , Font.family
            [ Font.external
                { url = "https://fonts.googleapis.com/css?family=Source+Code+Pro"
                , name = "Source Code Pro"
                }
            ]
        ]
        [ Element.text details.body ]
