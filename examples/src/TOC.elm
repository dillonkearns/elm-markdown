module TOC exposing (main)

import Element exposing (Element)
import Element.Font as Font
import ElmUi
import Html exposing (Attribute, Html)
import Markdown.Block exposing (Block, HeadingLevel, Inline)
import Markdown.Parser
import Markdown.Renderer


main : Html msg
main =
    Element.layout []
        (case view markdownBody of
            Ok ( toc, rendered ) ->
                Element.column
                    [ Element.spacing 30
                    , Element.padding 50
                    ]
                    (tocView toc
                        :: rendered
                    )

            Err errors ->
                Element.text errors
        )


tocView : TableOfContents -> Element msg
tocView toc =
    Element.column [ Element.alignTop, Element.spacing 20 ]
        [ Element.el [ Font.bold, Font.size 22 ] (Element.text "Table of Contents")
        , Element.column [ Element.spacing 10 ]
            (toc
                |> List.map
                    (\headingBlock ->
                        Element.link [ Font.color (Element.rgb255 100 100 100) ]
                            { url = "#" ++ headingBlock.anchorId
                            , label = Element.text headingBlock.name
                            }
                    )
            )
        ]


markdownBody =
    """
# Esse intrata referre inter adspeximus aequora soror

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

    var post = client_script_agp(bar_address_golden(flash, digital_halftone_unix
            + 3), supercomputer_bridge);
    type_ctr_drag(waveform, -2, core_rom);
    if (frameworkFile * wiki_ddr >= architecture_core(station_mebibyte, 5,
            dual_cd)) {
        tft.logMirror(infotainmentPram + spoofingArchitectureServer,
                sliRiscParse + mmsClean);
    }

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
                { anchorId = styledList |> Markdown.Block.extractInlineText |> rawTextToId
                , name = styledToString styledList
                , level = 1
                }
            )


styledToString : List Inline -> String
styledToString list =
    list
        |> Markdown.Block.extractInlineText


gatherHeadings : List Block -> List ( HeadingLevel, List Inline )
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


view : String -> Result String ( TableOfContents, List (Element msg) )
view markdown =
    case
        markdown
            |> Markdown.Parser.parse
    of
        Ok okAst ->
            case Markdown.Renderer.render renderer okAst of
                Ok rendered ->
                    Ok ( buildToc okAst, rendered )

                Err errors ->
                    Err errors

        Err error ->
            Err (error |> List.map Markdown.Parser.deadEndToString |> String.join "\n")


renderer : Markdown.Renderer.Renderer (Element msg)
renderer =
    ElmUi.renderer


rawTextToId : String -> String
rawTextToId rawText =
    rawText
        |> String.split " "
        |> String.join "-"
        |> String.toLower
