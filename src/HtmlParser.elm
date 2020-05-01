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
import Parser.Advanced as Advanced exposing ((|.), (|=), Nestable(..), Step(..), andThen, chompUntil, chompWhile, getChompedString, inContext, lazy, loop, map, oneOf, problem, succeed, token)
import Set exposing (Set)


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
    { name : String, value : String }


type alias Parser a =
    Advanced.Parser String Parser.Problem a


type alias DeadEnd =
    Advanced.DeadEnd String Parser.Problem


type Count
    = AtLeast Int


processingInstruction : Parser Node
processingInstruction =
    succeed ProcessingInstruction
        |. symbol "<?"
        |= getChompedString (Advanced.chompUntilEndOr "?>")
        |. symbol "?>"


notClosingBracket c =
    case c of
        ']' ->
            False

        _ ->
            True


notAmpersand c =
    case c of
        '&' ->
            False

        _ ->
            True


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
    keepOneOrMore (\c -> Char.isUpper c)


html : Parser Node
html =
    oneOf
        [ cdata |> map Cdata
        , processingInstruction
        , comment
        , docType
        , element
        ]


element : Advanced.Parser String Parser.Problem Node
element =
    succeed identity
        |. symbol "<"
        |= (tagName |> andThen elementContinuation)


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
    Advanced.chompIf tagNameCharacter (Parser.Expecting "at least one")
        |. chompWhile tagNameCharacter
        |> Advanced.mapChompedString (\name _ -> String.toLower name)


tagNameCharacter : Char -> Bool
tagNameCharacter c =
    -- inlined equivalent of `not (isWhitespace c) && isUninteresting c`
    case c of
        ' ' ->
            False

        '\u{000D}' ->
            False

        '\n' ->
            False

        '\t' ->
            False

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
    oneOf options
        |= succeed accum


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
        -- we can't use Advanced.token, because html tag names are supposed to
        -- be case-insensitive. So `<div>` could be closed by `</DIV>`.
        -- `tagName` normalizes to lowercase
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
textString end_ =
    keepZeroOrMore (\c -> c /= end_ && notAmpersand c)
        |> andThen
            (\s ->
                oneOf
                    [ succeed
                        (\c t ->
                            s ++ String.cons c t
                        )
                        |= escapedChar end_
                        |= lazy (\_ -> textString end_)
                    , succeed s
                    ]
            )



-- TEXT NODE STRING


textNodeString : Parser String
textNodeString =
    Advanced.loop () textNodeStringStep
        |> Advanced.getChompedString


textNodeStringStepOptions =
    [ Advanced.chompIf isNotTextNodeIgnoreChar (Parser.Expecting "is not & or <")
        |. chompWhile isNotTextNodeIgnoreChar
        |> Advanced.map (\_ -> Loop ())
    , escapedChar '<'
        |> Advanced.map (\_ -> Loop ())
    , succeed (Done ())
    ]


textNodeStringStep _ =
    oneOf textNodeStringStepOptions


isNotTextNodeIgnoreChar c =
    case c of
        '<' ->
            False

        '&' ->
            False

        _ ->
            True


notSemiColon c =
    case c of
        ';' ->
            False

        _ ->
            True


escapedChar : Char -> Parser Char
escapedChar end_ =
    succeed identity
        |. symbol "&"
        |= keepOneOrMore (\c -> c /= end_ && notSemiColon c)
        |> andThen
            (\s ->
                oneOf
                    [ symbol ";"
                        |> andThen
                            (\_ ->
                                case decodeEscape s of
                                    Ok c ->
                                        succeed c

                                    Err e ->
                                        problem e
                            )
                    , fail ("Entities must end_ with \";\": &" ++ s)
                    ]
            )


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
    -- attributesHelp Set.empty
    Advanced.loop Dict.empty attributesStep
        |> Advanced.map (Dict.foldl (\key value accum -> { name = key, value = value } :: accum) [])


attributesStep attrs =
    let
        -- when the same attribute is defined twice, the first definition is used
        updater new mValue =
            case mValue of
                Just v ->
                    Just v

                Nothing ->
                    Just new

        process name value =
            Loop (Dict.update (String.toLower name) (updater value) attrs)
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


attributeName : Parser String
attributeName =
    -- keepOneOrMore (\c -> not (isWhitespace c) && isUninteresting c)
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
    ignore oneOrMore isWhitespace


whiteSpace : Parser ()
whiteSpace =
    ignore zeroOrMore isWhitespace


isWhitespace : Char -> Bool
isWhitespace c =
    case c of
        ' ' ->
            True

        '\u{000D}' ->
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
                        -- |> Dict.foldl (\key value accum -> formatAttribute key value :: accum) []
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


maybe : Parser a -> Parser (Maybe a)
maybe parser =
    oneOf
        [ map Just parser
        , succeed Nothing
        ]


zeroOrMore : Count
zeroOrMore =
    AtLeast 0



--xmlParser : Parser Xml
--xmlParser =
--    xml


oneOrMore : Count
oneOrMore =
    AtLeast 1


keepZeroOrMore : (Char -> Bool) -> Parser String
keepZeroOrMore predicate =
    getChompedString (chompWhile predicate)


keepOneOrMore : (Char -> Bool) -> Parser String
keepOneOrMore predicate =
    Advanced.chompIf predicate (Parser.Expecting "at least one")
        |. chompWhile predicate
        |> getChompedString


keep : Count -> (Char -> Bool) -> Parser String
keep count predicate =
    case count of
        AtLeast 0 ->
            getChompedString (chompWhile predicate)

        AtLeast n ->
            getChompedString (chompWhile predicate)
                |> andThen
                    (\str ->
                        if n <= String.length str then
                            succeed str

                        else
                            problem Parser.BadRepeat
                    )


ignore : Count -> (Char -> Bool) -> Parser ()
ignore count predicate =
    case count of
        AtLeast 0 ->
            chompWhile predicate

        AtLeast n ->
            let
                checkLength startOffset endOffset =
                    if n <= (endOffset - startOffset) then
                        succeed ()

                    else
                        problem Parser.BadRepeat
            in
            succeed checkLength
                |= Advanced.getOffset
                |. chompWhile predicate
                |= Advanced.getOffset
                |> andThen identity


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
