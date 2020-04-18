module HtmlParser exposing
    ( Node(..), Attribute
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


{-| Attribute such as `name="value"`
-}
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
    inContext "processingInstruction" <|
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
    inContext "cdata" <|
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
    inContext "declaration" <|
        succeed Declaration
            |. symbol "<!"
            |= allUppercase
            |. oneOrMoreWhiteSpace
            |= getChompedString (Advanced.chompUntilEndOr ">")
            |. symbol ">"


allUppercase : Parser String
allUppercase =
    keep oneOrMore (\c -> Char.isUpper c)


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
    inContext
        "element"
    <|
        succeed identity
            |. symbol "<"
            |= (tagName
                    |> andThen
                        (\startTagName ->
                            succeed (Element startTagName)
                                |. whiteSpace
                                |= attributes Set.empty
                                |. whiteSpace
                                |= oneOf
                                    [ succeed []
                                        |. symbol "/>"
                                    , succeed identity
                                        |. symbol ">"
                                        |= lazy (\_ -> children startTagName)
                                    ]
                        )
               )


tagName : Parser String
tagName =
    map (\name -> String.toLower name) <|
        inContext "tagName" <|
            keep oneOrMore (\c -> not (isWhitespace c) && isUninteresting c)


children : String -> Parser (List Node)
children startTagName =
    inContext "children" <|
        oneOf
            [ succeed []
                |. closingTag startTagName
            , textNodeString
                |> andThen
                    (\maybeString ->
                        case maybeString of
                            Just s ->
                                succeed (\rest -> Text s :: rest)
                                    |= children startTagName

                            Nothing ->
                                succeed []
                                    |. closingTag startTagName
                    )
            , lazy
                (\_ ->
                    succeed (::)
                        |= html
                        |= children startTagName
                )
            ]


closingTag : String -> Parser ()
closingTag startTagName =
    inContext "closingTag" <|
        succeed ()
            |. symbol "</"
            |. whiteSpace
            |. (tagName
                    |> andThen
                        (\endTagName ->
                            if startTagName == endTagName then
                                succeed ()

                            else
                                fail ("tag name mismatch: " ++ startTagName ++ " and " ++ endTagName)
                        )
               )
            |. whiteSpace
            |. symbol ">"


textString : Char -> Parser String
textString end_ =
    inContext "textString" <|
        (keep zeroOrMore (\c -> c /= end_ && notAmpersand c)
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
        )


textNodeString : Parser (Maybe String)
textNodeString =
    -- Investigate: could we treat the empty string as no string?
    -- thereby removing the Maybe wrapper
    inContext "textNodeString" <|
        oneOf
            [ succeed
                (\s maybeString ->
                    Just (s ++ (maybeString |> Maybe.withDefault ""))
                )
                |= keep oneOrMore
                    (\c ->
                        case c of
                            '<' ->
                                False

                            '&' ->
                                False

                            _ ->
                                True
                    )
                |= lazy (\_ -> textNodeString)
            , succeed
                (\c maybeString ->
                    Just (String.cons c (maybeString |> Maybe.withDefault ""))
                )
                |= escapedChar '<'
                |= lazy (\_ -> textNodeString)
            , succeed Nothing
            ]


notSemiColon c =
    case c of
        ';' ->
            False

        _ ->
            True


escapedChar : Char -> Parser Char
escapedChar end_ =
    inContext "escapedChar" <|
        (succeed identity
            |. symbol "&"
            |= keep oneOrMore (\c -> c /= end_ && notSemiColon c)
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


attributes : Set String -> Parser (List Attribute)
attributes keys =
    map
        (\attrs ->
            List.map
                (\attr ->
                    { name = String.toLower attr.name
                    , value = attr.value
                    }
                )
                attrs
        )
    <|
        inContext
            "attributes"
        <|
            oneOf
                [ attribute
                    |> andThen
                        (\attr ->
                            if Set.member attr.name keys then
                                fail ("attribute " ++ attr.name ++ " is duplicated")

                            else
                                succeed ((::) attr)
                                    |. whiteSpace
                                    |= attributes (Set.insert attr.name keys)
                        )
                , succeed []
                ]


validateAttributes : Set String -> List Attribute -> Maybe String
validateAttributes keys attrs =
    case attrs of
        [] ->
            Nothing

        x :: xs ->
            if Set.member x.name keys then
                Just x.name

            else
                validateAttributes (Set.insert x.name keys) xs


attribute : Parser Attribute
attribute =
    inContext "attribute" <|
        succeed Attribute
            |= attributeName
            |. whiteSpace
            |. symbol "="
            |. whiteSpace
            |= attributeValue


attributeName : Parser String
attributeName =
    inContext "attributeName" <|
        keep oneOrMore (\c -> not (isWhitespace c) && isUninteresting c)


attributeValue : Parser String
attributeValue =
    inContext "attributeValue" <|
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
            "<"
                ++ escape tagName_
                ++ " "
                ++ (attributes_ |> List.map formatAttribute |> String.join " ")
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


formatAttribute : Attribute -> String
formatAttribute attribute_ =
    escape attribute_.name ++ "=\"" ++ escape attribute_.value ++ "\""


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



-- TODO make custom
-- zeroOrMore (c -> Bool) -> ...
-- oneOrMore (c -> Bool) -> ...
-- that'll remove the andThen from keep


zeroOrMore : Count
zeroOrMore =
    AtLeast 0



--xmlParser : Parser Xml
--xmlParser =
--    xml


oneOrMore : Count
oneOrMore =
    AtLeast 1


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
    Advanced.symbol (Advanced.Token str (Parser.ExpectingSymbol str))


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
