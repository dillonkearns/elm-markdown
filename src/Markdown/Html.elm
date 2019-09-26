module Markdown.Html exposing (..)

import List.Extra
import Markdown.Decoder


type alias Decoder a =
    Markdown.Decoder.Decoder a


type alias Attribute =
    { name : String, value : String }


mapDecoder : (a -> b) -> Decoder a -> Decoder b
mapDecoder function (Markdown.Decoder.Decoder handler) =
    (\tagName attributes innerBlocks ->
        handler tagName attributes innerBlocks
            |> Result.map function
    )
        |> Markdown.Decoder.Decoder


htmlSucceed : view -> Decoder view
htmlSucceed value =
    Markdown.Decoder.Decoder (\_ _ _ -> Ok value)


oneOf : List (Decoder view) -> Decoder view
oneOf decoders =
    let
        unwrappedDecoders =
            decoders
                |> List.map
                    (\(Markdown.Decoder.Decoder rawDecoder) -> rawDecoder)
    in
    List.foldl
        (\decoder soFar ->
            \tagName attributes children ->
                resultOr (decoder tagName attributes children) (soFar tagName attributes children)
        )
        (\tagName attributes children ->
            Err []
        )
        unwrappedDecoders
        |> (\rawDecoder ->
                (\tagName attributes innerBlocks ->
                    rawDecoder tagName attributes innerBlocks
                        |> Result.mapError
                            (\errors ->
                                case errors of
                                    [] ->
                                        "Ran into a oneOf with no possibilities!"

                                    [ singleError ] ->
                                        """Problem with the given value:

"""
                                            ++ tagToString tagName attributes
                                            ++ "\n\n"
                                            ++ singleError
                                            ++ "\n"

                                    _ ->
                                        """oneOf failed parsing this value:
    """
                                            ++ tagToString tagName attributes
                                            ++ """

Parsing failed in the following 2 ways:


"""
                                            ++ (List.indexedMap
                                                    (\index error ->
                                                        "("
                                                            ++ String.fromInt (index + 1)
                                                            ++ ") "
                                                            ++ error
                                                    )
                                                    errors
                                                    |> String.join "\n\n"
                                               )
                                            ++ "\n"
                            )
                )
                    |> Markdown.Decoder.Decoder
           )


resultOr : Result e a -> Result (List e) a -> Result (List e) a
resultOr ra rb =
    case ra of
        Err singleError ->
            case rb of
                Ok okValue ->
                    Ok okValue

                Err errorsSoFar ->
                    Err (singleError :: errorsSoFar)

        Ok okValue ->
            Ok okValue


tagToString : String -> List Attribute -> String
tagToString tagName attributes =
    "<" ++ tagName ++ ">"


tag : String -> view -> Decoder view
tag expectedTag a =
    Markdown.Decoder.Decoder
        (\tagName attributes children ->
            if tagName == expectedTag then
                Ok a

            else
                Err ("Expected " ++ expectedTag ++ " but was " ++ tagName)
        )


withAttribute : String -> Decoder (String -> view) -> Decoder view
withAttribute attributeName (Markdown.Decoder.Decoder handler) =
    (\tagName attributes innerBlocks ->
        handler tagName attributes innerBlocks
            |> (case
                    attributes
                        |> List.Extra.find
                            (\{ name, value } ->
                                name == attributeName
                            )
                of
                    Just { value } ->
                        Result.map ((|>) value)

                    Nothing ->
                        \_ ->
                            Err
                                ("Expecting attribute \""
                                    ++ attributeName
                                    ++ "\"."
                                )
               )
    )
        |> Markdown.Decoder.Decoder
