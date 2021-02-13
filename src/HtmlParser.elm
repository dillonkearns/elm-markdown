module HtmlParser exposing
    ( Node(..)
    , Parser, html
    )

{-| Parses HTML, specifically for Markdown. Which may have some differences from parsing a full HTML document.


# Types

@docs Html, ProcessingInstruction, DocType, DocTypeDefinition, Node, Attribute


# Parse

@docs parse


# Format

@docs format

-}

import Char
import Dict exposing (Dict)
import Hex
import Parser as Parser
import Parser.Advanced as Advanced exposing ((|.), (|=), Nestable(..), Step(..), andThen, chompWhile, getChompedString, loop, map, oneOf, problem, succeed, token)


{-| Node is either a element such as `<a name="value">foo</a>` or text such as `foo`.
-}
type Node
    = Element String (List Attribute) (List Node)
    | Text String
    | Comment String
    | Cdata String
    | ProcessingInstruction String
    | Declaration String String


type alias Attribute =
    { name : String
    , value : String
    }


type alias Parser a =
    Advanced.Parser String Parser.Problem a


processingInstruction : Parser Node
processingInstruction =
    succeed ProcessingInstruction
        |. symbol "<?"
        |= getChompedString (Advanced.chompUntilEndOr "?>")
        |. symbol "?>"


cdata : Parser String
cdata =
    succeed identity
        |. symbol "<![CDATA["
        |= getChompedString (Advanced.chompUntilEndOr "]]>")
        |. symbol "]]>"


docType : Parser Node
docType =
    {-
       <!
       a name consisting of one or more uppercase ASCII letters
       whitespace
       a string of characters not including the character >
       and the character >
    -}
    succeed Declaration
        |. symbol "<!"
        |= allUppercase
        |. oneOrMoreWhiteSpace
        |= getChompedString (Advanced.chompUntilEndOr ">")
        |. symbol ">"


allUppercase : Parser String
allUppercase =
    Advanced.chompIf Char.isUpper expectUppercaseCharacter
        |. chompWhile Char.isUpper
        |> getChompedString


expectUppercaseCharacter : Parser.Problem
expectUppercaseCharacter =
    Parser.Expecting "at least 1 uppercase character"


html : Parser Node
html =
    oneOf
        [ cdata |> map Cdata
        , processingInstruction
        , comment
        , docType
        , element
        ]


element : Parser Node
element =
    succeed identity
        |. symbol "<"
        |= (tagName |> andThen elementContinuation)


elementContinuation : String -> Parser Node
elementContinuation startTagName =
    succeed (Element startTagName)
        |. whiteSpace
        |= attributes
        |. whiteSpace
        |= oneOf
            [ symbol "/>"
                |> Advanced.map (\_ -> [])
            , succeed identity
                |. symbol ">"
                |= children startTagName
            ]


tagName : Parser String
tagName =
    Advanced.chompIf tagNameCharacter expectTagNameCharacter
        |. chompWhile tagNameCharacter
        |> Advanced.mapChompedString (\name _ -> String.toLower name)


expectTagNameCharacter : Parser.Problem
expectTagNameCharacter =
    Parser.Expecting "at least 1 tag name character"


tagNameCharacter : Char -> Bool
tagNameCharacter c =
    -- inlined equivalent of `not (isWhitespace c) && isUninteresting c`
    case c of
        -- whitespace chars
        ' ' ->
            False

        '\r' ->
            False

        '\n' ->
            False

        '\t' ->
            False

        -- html structural characters
        '/' ->
            False

        '<' ->
            False

        '>' ->
            False

        '"' ->
            False

        '\'' ->
            False

        '=' ->
            False

        _ ->
            True


children : String -> Parser (List Node)
children startTagName =
    Advanced.loop [] (childrenStep (childrenStepOptions startTagName))


childrenStep : List (Parser (List Node -> Step (List Node) (List Node))) -> List Node -> Parser (Step (List Node) (List Node))
childrenStep options accum =
    -- This weird construction is so the `childrenStepOptions` can be shared by all iterations,
    -- rather than be re-defined on every iteration
    oneOf options
        |> map (\f -> f accum)


childrenStepOptions : String -> List (Parser (List Node -> Step (List Node) (List Node)))
childrenStepOptions startTagName =
    [ closingTag startTagName
        |> Advanced.map (\_ accum -> Done (List.reverse accum))
    , textNodeString
        |> andThen
            (\text ->
                if String.isEmpty text then
                    closingTag startTagName
                        |> Advanced.map (\_ accum -> Done (List.reverse accum))

                else
                    succeed (\accum -> Loop (Text text :: accum))
            )
    , html
        |> Advanced.map (\new accum -> Loop (new :: accum))
    ]


closingTag : String -> Parser ()
closingTag startTagName =
    let
        -- we can't use Advanced.token, because html tag names are case-insensitive.
        --So `<div>` could be closed by `</DIV>`. `tagName` normalizes to lowercase
        closingTagName =
            tagName
                |> andThen
                    (\endTagName ->
                        if startTagName == endTagName then
                            succeed ()

                        else
                            fail ("tag name mismatch: " ++ startTagName ++ " and " ++ endTagName)
                    )
    in
    symbol "</"
        |. whiteSpace
        |. closingTagName
        |. whiteSpace
        |. symbol ">"


textString : Char -> Parser String
textString closingChar =
    let
        predicate c =
            c /= closingChar && c /= '&'
    in
    Advanced.loop "" (textStringStep closingChar predicate)


textStringStep : Char -> (Char -> Bool) -> String -> Parser (Step String String)
textStringStep closingChar predicate accum =
    chompWhile predicate
        |> getChompedString
        |> andThen
            (\soFar ->
                oneOf
                    [ escapedChar closingChar
                        |> map (\escaped -> Loop (accum ++ soFar ++ String.fromChar escaped))
                    , succeed (Done (accum ++ soFar))
                    ]
            )



-- TEXT NODE STRING


textNodeString : Parser String
textNodeString =
    Advanced.loop () textNodeStringStep
        |> Advanced.getChompedString


textNodeStringStepOptions : List (Parser (Step () ()))
textNodeStringStepOptions =
    [ Advanced.chompIf isNotTextNodeIgnoreChar (Parser.Expecting "is not & or <")
        |. chompWhile isNotTextNodeIgnoreChar
        |> Advanced.map (\_ -> Loop ())
    , escapedChar '<'
        |> Advanced.map (\_ -> Loop ())
    , succeed (Done ())
    ]


textNodeStringStep : () -> Parser (Step () ())
textNodeStringStep _ =
    oneOf textNodeStringStepOptions


isNotTextNodeIgnoreChar : Char -> Bool
isNotTextNodeIgnoreChar c =
    case c of
        '<' ->
            False

        '&' ->
            False

        _ ->
            True


escapedChar : Char -> Parser Char
escapedChar end_ =
    let
        isEntityChar c =
            c /= end_ && c /= ';'

        process entityStr =
            case decodeEscape entityStr of
                Ok c ->
                    succeed c

                Err e ->
                    problem e
    in
    succeed identity
        |. symbol "&"
        |= (Advanced.chompIf isEntityChar (Parser.Expecting "an entity character")
                |. chompWhile isEntityChar
                |> Advanced.getChompedString
                |> andThen process
           )
        |. symbol ";"


decodeEscape : String -> Result Parser.Problem Char
decodeEscape s =
    if String.startsWith "#x" s then
        s
            |> String.dropLeft 2
            |> Hex.fromString
            |> Result.map Char.fromCode
            |> Result.mapError Parser.Problem

    else if String.startsWith "#" s then
        s
            |> String.dropLeft 1
            |> String.toInt
            |> Maybe.map Char.fromCode
            |> Result.fromMaybe (Parser.Problem <| "Invalid escaped character: " ++ s)

    else
        Dict.get s entities
            |> Result.fromMaybe (Parser.Problem <| "No entity named \"&" ++ s ++ ";\" found.")


entities : Dict String Char
entities =
    Dict.fromList
        [ ( "amp", '&' )
        , ( "lt", '<' )
        , ( "gt", '>' )
        , ( "apos", '\'' )
        , ( "quot", '"' )
        ]


attributes : Parser (List Attribute)
attributes =
    Advanced.loop Dict.empty attributesStep
        |> Advanced.map (Dict.foldl (\key value accum -> { name = key, value = value } :: accum) [])


attributesStep : Dict String String -> Parser (Step (Dict String String) (Dict String String))
attributesStep attrs =
    let
        -- when the same attribute is defined twice, the first definition is used
        process name value =
            Loop (Dict.update (String.toLower name) (keepOldest value) attrs)
    in
    oneOf
        [ succeed process
            |= attributeName
            |. whiteSpace
            |. symbol "="
            |. whiteSpace
            |= attributeValue
            |. whiteSpace
        , succeed (Done attrs)
        ]


keepOldest : a -> Maybe a -> Maybe a
keepOldest new mValue =
    case mValue of
        Just v ->
            Just v

        Nothing ->
            Just new


attributeName : Parser String
attributeName =
    tagName


attributeValue : Parser String
attributeValue =
    oneOf
        [ succeed identity
            |. symbol "\""
            |= textString '"'
            |. symbol "\""
        , succeed identity
            |. symbol "'"
            |= textString '\''
            |. symbol "'"
        ]


oneOrMoreWhiteSpace : Parser ()
oneOrMoreWhiteSpace =
    Advanced.chompIf isWhitespace (Parser.Expecting "at least one whitespace")
        |. chompWhile isWhitespace


whiteSpace : Parser ()
whiteSpace =
    chompWhile isWhitespace


isWhitespace : Char -> Bool
isWhitespace c =
    case c of
        ' ' ->
            True

        '\r' ->
            True

        '\n' ->
            True

        '\t' ->
            True

        _ ->
            False


comment : Parser Node
comment =
    succeed Comment
        |. token (toToken "<!--")
        |= getChompedString (Advanced.chompUntilEndOr "-->")
        |. token (toToken "-->")



-- FORMAT


formatNode : Node -> String
formatNode node =
    case node of
        Element tagName_ attributes_ children_ ->
            let
                formattedAttributes =
                    attributes_
                        |> List.map (\{ name, value } -> formatAttribute name value)
                        |> String.join " "
            in
            "<"
                ++ escape tagName_
                ++ " "
                ++ formattedAttributes
                ++ (if children_ == [] then
                        "/>"

                    else
                        ">"
                            ++ (children_ |> List.map formatNode |> String.join "")
                            ++ "</"
                            ++ escape tagName_
                            ++ ">"
                   )

        Text s ->
            escape s

        Comment commentContents ->
            String.concat
                [ "<!-- "
                , commentContents
                , " -->"
                ]

        Cdata string ->
            String.concat
                [ "<![CDATA["
                , string
                , "]"
                ]

        ProcessingInstruction string ->
            String.concat
                [ "<?"
                , string
                , "?>"
                ]

        Declaration declarationType content ->
            String.concat
                [ "<!"
                , declarationType
                , " "
                , content
                , ">"
                ]


formatAttribute : String -> String -> String
formatAttribute name value =
    escape name ++ "=\"" ++ escape value ++ "\""


escape : String -> String
escape s =
    s
        |> String.replace "&" "&amp;"
        |> String.replace "<" "&lt;"
        |> String.replace ">" "&gt;"
        |> String.replace "\"" "&quot;"
        |> String.replace "'" "&apos;"



-- UTILITY


fail : String -> Parser a
fail str =
    problem (Parser.Problem str)


symbol : String -> Parser ()
symbol str =
    Advanced.token (Advanced.Token str (Parser.ExpectingSymbol str))


toToken : String -> Advanced.Token Parser.Problem
toToken str =
    Advanced.Token str (Parser.Expecting str)


isUninteresting : Char -> Bool
isUninteresting c =
    case c of
        '/' ->
            False

        '<' ->
            False

        '>' ->
            False

        '"' ->
            False

        '\'' ->
            False

        '=' ->
            False

        _ ->
            True
