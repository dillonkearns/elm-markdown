module Markdown.Helpers exposing (..)

import Dict exposing (Dict)
import Markdown.Entity as Entity
import Regex exposing (Regex)


type alias References =
    Dict String ( String, Maybe String )



-- Label ( Url, Maybe Title )


type alias Attribute =
    { name : String, value : String }


insideSquareBracketRegex : String
insideSquareBracketRegex =
    "[^\\[\\]\\\\]*(?:\\\\.[^\\[\\]\\\\]*)*"


titleRegex : String
titleRegex =
    "(?:["
        ++ whiteSpaceChars
        ++ "]+"
        ++ "(?:'([^'\\\\]*(?:\\\\.[^'\\\\]*)*)'|"
        ++ "\"([^\"\\\\]*(?:\\\\.[^\"\\\\]*)*)\"|"
        ++ "\\(([^\\)\\\\]*(?:\\\\.[^\\)\\\\]*)*)\\)))?"


lineEndChars : String
lineEndChars =
    "\\f\\v\\r\\n"


whiteSpaceChars : String
whiteSpaceChars =
    " \\t\\f\\v\\r\\n"


prepareRefLabel : String -> String
prepareRefLabel =
    cleanWhitespaces >> String.toLower


cleanWhitespaces : String -> String
cleanWhitespaces original =
    original



--|> String.trim
--|> Regex.replace whitespacesRegex (\_ -> " ")


whitespacesRegex : Regex
whitespacesRegex =
    Regex.fromString ("[" ++ whiteSpaceChars ++ "]+")
        |> Maybe.withDefault Regex.never


tabRegex : Regex
tabRegex =
    Regex.fromString "\\t"
        |> Maybe.withDefault Regex.never


containsAmpersand : String -> Bool
containsAmpersand string =
    String.contains "&" string


initSpacesRegex : Regex
initSpacesRegex =
    Regex.fromString "^ +"
        |> Maybe.withDefault Regex.never


formatStr : String -> String
formatStr str =
    let
        withEscapes =
            replaceEscapable str
    in
    -- check if there is an  ampersand in the string before attempting expensive regexes
    if containsAmpersand withEscapes then
        withEscapes
            |> Entity.replaceEntities
            |> Entity.replaceDecimals
            |> Entity.replaceHexadecimals

    else
        withEscapes


escapableRegex : Regex
escapableRegex =
    Regex.fromString "(\\\\+)([!\"#$%&\\'()*+,./:;<=>?@[\\\\\\]^_`{|}~-])"
        |> Maybe.withDefault Regex.never


replaceEscapable : String -> String
replaceEscapable =
    Regex.replace escapableRegex
        (\regexMatch ->
            case regexMatch.submatches of
                (Just backslashes) :: (Just escapedStr) :: _ ->
                    String.repeat (String.length backslashes // 2) "\\" ++ escapedStr

                _ ->
                    regexMatch.match
        )


returnFirstJust : List (Maybe a) -> Maybe a
returnFirstJust maybes =
    let
        process : Maybe a -> Maybe a -> Maybe a
        process a maybeFound =
            case maybeFound of
                Just found ->
                    Just found

                Nothing ->
                    a
    in
    List.foldl process Nothing maybes


ifError : (x -> Result x a) -> Result x a -> Result x a
ifError function result =
    case result of
        Result.Ok _ ->
            result

        Result.Err err ->
            function err


isEven : Int -> Bool
isEven int =
    modBy 2 int == 0
