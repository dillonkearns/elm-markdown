module Markdown.Parser exposing (..)

import Parser
import Parser.Advanced as Advanced
    exposing
        ( (|.)
        , (|=)
        , Nestable(..)
        , Step(..)
        , andThen
        , chompUntil
        , chompWhile
        , getChompedString
        , inContext
        , int
        , lazy
        , loop
        , map
        , multiComment
        , oneOf
        , problem
        , succeed
        , symbol
        , token
        )
import XmlParser exposing (Node(..))


type Decoder a
    = Decoder (HtmlNode -> Result String a)


htmlSucceed : view -> Decoder view
htmlSucceed value =
    Decoder (\_ -> Ok value)


htmlTag : String -> view -> Decoder view
htmlTag expectedTag a =
    Decoder
        (\node ->
            case node of
                Element tag attributes children ->
                    if tag == expectedTag then
                        Ok a

                    else
                        Err ("Expected " ++ expectedTag ++ " but was " ++ tag)

                _ ->
                    Err ("Was expecting " ++ expectedTag ++ " but this is not a tag... Should be impossible TODO")
        )


type alias Renderer view =
    { h1 : String -> view
    , h2 : String -> view
    , raw : String -> view
    , todo : view
    , htmlDecoder : Decoder (List view -> view)
    }


renderHelper :
    Renderer view
    -> List Block
    -> List (Result String view)
renderHelper renderer blocks =
    List.map
        (\block ->
            case block of
                Heading 1 content ->
                    renderer.h1 content
                        |> Ok

                Heading 2 content ->
                    renderer.h2 content
                        |> Ok

                Heading level content ->
                    -- TODO
                    renderer.h2 content
                        |> Ok

                Body content ->
                    renderer.raw content
                        |> Ok

                Html html ->
                    renderHtmlNode renderer html
        )
        blocks


render :
    Renderer view
    -> String
    -> Result String (List view)
render renderer markdownText =
    markdownText
        |> parse
        |> Result.mapError deadEndsToString
        |> Result.andThen
            (\markdownAst ->
                markdownAst
                    |> renderHelper renderer
                    |> combineResults
            )


combineResults : List (Result x a) -> Result x (List a)
combineResults =
    List.foldr (Result.map2 (::)) (Ok [])


deadEndsToString deadEnds =
    "Errors"


renderHtmlNode : Renderer view -> HtmlNode -> Result String view
renderHtmlNode renderer html =
    case html of
        InnerBlocks innerBlocks ->
            renderHelper renderer innerBlocks
                |> useRed html renderer.htmlDecoder

        Element tag attributes children ->
            List.map (renderHtmlNode renderer) children
                |> useRed html renderer.htmlDecoder


useRed : HtmlNode -> Decoder (List view -> view) -> List (Result String view) -> Result String view
useRed htmlNode (Decoder redRenderer) renderedChildren =
    renderedChildren
        |> combineResults
        |> (\childrenResult ->
                case childrenResult of
                    Ok okChildren ->
                        redRenderer htmlNode
                            |> Result.map
                                (\myRenderer -> myRenderer okChildren)

                    Err errors ->
                        Err errors
           )


type alias Parser a =
    Advanced.Parser String Parser.Problem a


type Block
    = Heading Int String
    | Body String
    | Html HtmlNode


type alias Attribute =
    { name : String, value : String }


type HtmlNode
    = Element String (List Attribute) (List HtmlNode)
      -- | Text String
    | InnerBlocks (List Block)


body : Parser Block
body =
    succeed Body
        |= getChompedString
            (succeed ()
                |. chompWhile (\c -> c /= '\n')
            )


lineParser : Parser Block
lineParser =
    oneOf
        [ heading
        , htmlParser
        , body
        ]


htmlParser : Parser Block
htmlParser =
    XmlParser.element
        |> xmlNodeToHtmlNode
        |> Advanced.map Html


xmlNodeToHtmlNode : Parser Node -> Parser HtmlNode
xmlNodeToHtmlNode parser =
    Advanced.andThen
        (\xmlNode ->
            case xmlNode of
                XmlParser.Text innerText ->
                    InnerBlocks [ Body innerText ]
                        |> Advanced.succeed

                XmlParser.Element tag attributes children ->
                    Advanced.andThen
                        (\parsedChildren ->
                            Advanced.succeed
                                (Element tag
                                    attributes
                                    parsedChildren
                                )
                        )
                        (thing children)
        )
        parser


thing : List Node -> Parser (List HtmlNode)
thing children =
    children
        |> List.map childToParser
        |> combine


combine : List (Parser a) -> Parser (List a)
combine list =
    list
        |> List.foldl
            (\parser listParser ->
                listParser
                    |> Advanced.andThen
                        (\soFar ->
                            parser
                                |> Advanced.map (\a -> a :: soFar)
                        )
            )
            (Advanced.succeed [])


childToParser : Node -> Parser HtmlNode
childToParser node =
    case node of
        XmlParser.Element tag attributes [] ->
            Advanced.succeed (Element tag attributes [])

        Text innerText ->
            case Advanced.run multiParser innerText of
                Ok value ->
                    succeed (InnerBlocks value)

                Err error ->
                    Advanced.problem (Parser.Expecting (error |> Debug.toString))

        -- |> Advanced.map Html
        _ ->
            Debug.todo "handle Element _ _ (_ :: _)"


multiParser : Parser (List Block)
multiParser =
    loop [] statementsHelp


statementsHelp : List Block -> Parser (Step (List Block) (List Block))
statementsHelp revStmts =
    oneOf
        [ succeed (\stmt -> Loop (stmt :: revStmts))
            |= lineParser
            |. symbol (Advanced.Token "\n" (Parser.Expecting "newline"))
        , succeed ()
            |> map (\_ -> Done (List.reverse revStmts))
        ]


heading : Parser Block
heading =
    succeed Heading
        |. symbol (Advanced.Token "#" (Parser.Expecting "#"))
        |= (getChompedString
                (succeed ()
                    |. chompWhile (\c -> c == '#')
                )
                |> map
                    (\additionalHashes ->
                        String.length additionalHashes + 1
                    )
           )
        |. chompWhile (\c -> c == ' ')
        |= getChompedString (succeed () |. chompWhile (\c -> c /= '\n'))


parse : String -> Result (List (Advanced.DeadEnd String Parser.Problem)) (List Block)
parse input =
    Advanced.run multiParser input
