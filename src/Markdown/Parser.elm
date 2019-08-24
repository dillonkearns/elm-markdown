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
    = Decoder (String -> List Attribute -> List Block -> Result String a)


htmlSucceed : view -> Decoder view
htmlSucceed value =
    Decoder (\_ _ _ -> Ok value)


htmlOneOf : List (Decoder view) -> Decoder view
htmlOneOf decoders =
    List.foldl
        (\(Decoder decoder) (Decoder soFar) ->
            Decoder
                (\tag attributes children ->
                    resultOr (decoder tag attributes children) (soFar tag attributes children)
                )
        )
        (Decoder (\tag attributes children -> Err "No Html Decoders succeeded in oneOf."))
        decoders



-- (\decoder soFar -> soFar)
-- (\_ _ -> Debug.todo "")
-- (\node -> Err "No decoders")
-- decoders


resultOr : Result e a -> Result e a -> Result e a
resultOr ra rb =
    case ra of
        Err _ ->
            rb

        Ok _ ->
            ra


htmlTag : String -> view -> Decoder view
htmlTag expectedTag a =
    Decoder
        (\tag attributes children ->
            if tag == expectedTag then
                Ok a

            else
                Err ("Expected " ++ expectedTag ++ " but was " ++ tag)
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

                Html tag attributes children ->
                    renderHtmlNode renderer tag attributes children
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


renderHtmlNode : Renderer view -> String -> List Attribute -> List Block -> Result String view
renderHtmlNode renderer tag attributes children =
    useRed tag
        attributes
        children
        -- TODO why am I passing children AND parsed children... this is probably causing a bug
        renderer.htmlDecoder
        (renderHelper renderer children)


useRed : String -> List Attribute -> List Block -> Decoder (List view -> view) -> List (Result String view) -> Result String view
useRed tag attributes children (Decoder redRenderer) renderedChildren =
    renderedChildren
        |> combineResults
        |> Result.andThen
            (\okChildren ->
                redRenderer tag attributes children
                    |> Result.map
                        (\myRenderer -> myRenderer okChildren)
            )


type alias Parser a =
    Advanced.Parser String Parser.Problem a


type Block
    = Heading Int String
    | Body String
    | Html String (List Attribute) (List Block)


type alias Attribute =
    { name : String, value : String }


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



-- |> Advanced.map toTopLevelHtml


toTopLevelHtml : String -> List Attribute -> List Block -> Block
toTopLevelHtml tag attributes children =
    Html tag attributes children


xmlNodeToHtmlNode : Parser Node -> Parser Block
xmlNodeToHtmlNode parser =
    Advanced.andThen
        (\xmlNode ->
            case xmlNode of
                XmlParser.Text innerText ->
                    -- TODO is this right?
                    Body innerText
                        |> Advanced.succeed

                XmlParser.Element tag attributes children ->
                    Advanced.andThen
                        (\parsedChildren ->
                            Advanced.succeed
                                (Html tag
                                    attributes
                                    parsedChildren
                                )
                        )
                        (nodesToBlocksParser children)
        )
        parser


nodesToBlocksParser : List Node -> Parser (List Block)
nodesToBlocksParser children =
    children
        |> List.map childToParser
        |> combine
        |> Advanced.map List.concat


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


childToParser : Node -> Parser (List Block)
childToParser node =
    case node of
        XmlParser.Element tag attributes [] ->
            -- TODO
            Advanced.succeed [ Html tag attributes [] ]

        Text innerText ->
            case Advanced.run multiParser innerText of
                Ok value ->
                    succeed value

                Err error ->
                    Advanced.problem (Parser.Expecting (error |> Debug.toString))

        Element tag attributes children ->
            nodesToBlocksParser children
                |> Advanced.andThen
                    (\childrenAsBlocks ->
                        Advanced.succeed [ Html tag attributes childrenAsBlocks ]
                    )


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
