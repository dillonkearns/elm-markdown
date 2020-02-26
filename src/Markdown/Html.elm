module Markdown.Html exposing
    ( Renderer
    , tag, withAttribute, withOptionalAttribute
    , map, oneOf
    )

{-|

@docs Renderer


## Creating an HTML renderer

@docs tag, withAttribute, withOptionalAttribute
@docs map, oneOf

-}

import Html
import List.Helpers
import Markdown.Block exposing (Block)
import Markdown.HtmlRenderer


{-| A `Markdown.Html.Renderer` is how you register the list of
valid HTML tags that can be used in your markdown. A `Renderer`
also defines how to render those tags that it accepts.

Using an HTML renderer feels similar to building a JSON decoder.
You're describing what kind of data you expect to have. You
also provide functions that tell what to do with those bits of data.

For example, if you expect to have an attribute called `button-text` for the
`<signup-form ...>` tags in your Markdown, you could use the value of the
`button-text` attribute to render your `<signup-form` like so

-}
type alias Renderer a =
    Markdown.HtmlRenderer.HtmlRenderer a


type alias Attribute =
    { name : String, value : String }


{-| Map the value of a `Markdown.Html.Renderer`.
-}
map : (a -> b) -> Renderer a -> Renderer b
map function (Markdown.HtmlRenderer.HtmlRenderer renderer) =
    (\tagName attributes innerBlocks ->
        renderer tagName attributes innerBlocks
            |> Result.map function
    )
        |> Markdown.HtmlRenderer.HtmlRenderer


{-| Usually you want to handle a list of possible HTML
tags, not just a single one. So 99% of the time you'll
be using this function when you use this module.

    htmlRenderer =
        Markdown.Html.oneOf
            [ Markdown.Html.tag "contact-button"
                (\children -> contactButtonView)
            , Markdown.Html.tag "signup-form"
                (\children -> signupFormView children)
            ]

-}
oneOf : List (Renderer view) -> Renderer view
oneOf decoders =
    let
        unwrappedDecoders =
            decoders
                |> List.map
                    (\(Markdown.HtmlRenderer.HtmlRenderer rawDecoder) -> rawDecoder)
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
                    |> Markdown.HtmlRenderer.HtmlRenderer
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
    if List.isEmpty attributes then
        "<" ++ tagName ++ ">"

    else
        "<" ++ tagName ++ " " ++ attributesToString attributes ++ ">"


attributesToString : List Attribute -> String
attributesToString attributes =
    attributes
        |> List.map
            (\{ name, value } ->
                name ++ "=\"" ++ value ++ "\""
            )
        |> String.join " "


{-| Start a Renderer by expecting a tag of a particular type.

    Markdown.Html.tag "contact-button"
        (\children ->
            -- we don't want to use any inner markdown
            -- within <contact-button> tags, so we'll
            -- ignore this argument
            Html.button
         -- ... fancy SVG and mailto links here
        )

-}
tag : String -> view -> Renderer view
tag expectedTag a =
    Markdown.HtmlRenderer.HtmlRenderer
        (\tagName attributes children ->
            if tagName == expectedTag then
                Ok a

            else
                Err ("Expected " ++ expectedTag ++ " but was " ++ tagName)
        )


{-| Expects an attribute. The `Renderer` will fail if that attribute doesn't
exist on the tag. You can use the values of all the expected tags in the function
you define for the tag's renderer.

    import Html
    import Html.Attributes as Attr
    import Markdown.Html

    Markdown.Html.tag "contact-button"
        (\children buttonText color ->
            Html.button
                [ Attr.style "background-color" color ]
                [ Html.text buttonText ]
        )
        |> Markdown.Html.withAttribute "button-text"
        |> Markdown.Html.withAttribute "color"

-}
withAttribute : String -> Renderer (String -> view) -> Renderer view
withAttribute attributeName (Markdown.HtmlRenderer.HtmlRenderer renderer) =
    (\tagName attributes innerBlocks ->
        renderer tagName attributes innerBlocks
            |> (case
                    attributes
                        |> List.Helpers.find
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
        |> Markdown.HtmlRenderer.HtmlRenderer


{-| Same as `withAttribute`, but the Renderer won't fail if the attribute is missing.
Instead, it just returns `Nothing` for missing attributes.

    import Html
    import Html.Attributes as Attr
    import Markdown.Html

    Markdown.Html.tag "bio"
        (\name twitter github children ->
            bioView name twitter github children
        )

-}
withOptionalAttribute : String -> Renderer (Maybe String -> view) -> Renderer view
withOptionalAttribute attributeName (Markdown.HtmlRenderer.HtmlRenderer renderer) =
    (\tagName attributes innerBlocks ->
        renderer tagName attributes innerBlocks
            |> (case
                    attributes
                        |> List.Helpers.find
                            (\{ name, value } ->
                                name == attributeName
                            )
                of
                    Just { value } ->
                        Result.map ((|>) (Just value))

                    Nothing ->
                        Result.map ((|>) Nothing)
               )
    )
        |> Markdown.HtmlRenderer.HtmlRenderer
