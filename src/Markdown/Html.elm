module Markdown.Html exposing
    ( Renderer
    , tag, withAttribute, withOptionalAttribute
    , map, oneOf, oneOfWithFallback
    , TagFilter, safeTags, allowTags, denyTags
    )

{-|

@docs Renderer


## Creating an HTML renderer

@docs tag, withAttribute, withOptionalAttribute
@docs map, oneOf, oneOfWithFallback


## Tag Filtering

@docs TagFilter, safeTags, allowTags, denyTags


-}

import List.Helpers
import Markdown.Block exposing (Block)
import Markdown.HtmlRenderer
import Set exposing (Set)


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
        unwrappedDecoders : List (String -> List Markdown.HtmlRenderer.Attribute -> List Block -> Result String view)
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
        (\_ _ _ ->
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
        (\tagName _ _ ->
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
        (\buttonText color children ->
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
                            (\{ name } ->
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


{-| An opaque type that controls which HTML tags are allowed through the fallback
in [`oneOfWithFallback`](#oneOfWithFallback). See [`safeTags`](#safeTags),
[`allowTags`](#allowTags), and [`denyTags`](#denyTags).
-}
type TagFilter
    = TagFilter (String -> Bool)


{-| A tag filter that allows most tags but excludes the tags disallowed by the
[GFM Disallowed Raw HTML extension (section 6.11)](https://github.github.com/gfm/#disallowed-raw-html-extension-):
`title`, `textarea`, `style`, `xmp`, `iframe`, `noembed`, `noframes`, `script`, `plaintext`.

These tags are excluded because they change how HTML is interpreted in ways that
are usually undesirable in the context of rendered Markdown content.

-}
safeTags : TagFilter
safeTags =
    let
        unsafeTags : Set String
        unsafeTags =
            Set.fromList
                [ "title"
                , "textarea"
                , "style"
                , "xmp"
                , "iframe"
                , "noembed"
                , "noframes"
                , "script"
                , "plaintext"
                ]
    in
    TagFilter (\tagName -> not (Set.member tagName unsafeTags))


{-| A tag filter that only allows the specified tags through the fallback.

    Markdown.Html.allowTags [ "div", "span", "details", "summary" ]

-}
allowTags : List String -> TagFilter
allowTags allowed =
    let
        allowedSet : Set String
        allowedSet =
            Set.fromList allowed
    in
    TagFilter (\tagName -> Set.member tagName allowedSet)


{-| A tag filter that allows all tags except the specified ones.

    Markdown.Html.denyTags [ "script", "iframe" ]

-}
denyTags : List String -> TagFilter
denyTags denied =
    let
        deniedSet : Set String
        deniedSet =
            Set.fromList denied
    in
    TagFilter (\tagName -> not (Set.member tagName deniedSet))


{-| Like [`oneOf`](#oneOf), but with a fallback for tags not matched by any specific renderer.

The fallback function receives the tag name, attributes, and rendered children.
If it returns `Nothing`, or if the `TagFilter` rejects the tag, the tag is escaped
as text using the provided text function.

    htmlRenderer =
        Markdown.Html.oneOfWithFallback
            [ Markdown.Html.tag "custom-widget" (\children -> myWidget children)
            ]
            Markdown.Html.safeTags
            Html.text
            (\tag attributes children ->
                Just (Html.node tag (attributesToHtmlAttrs attributes) children)
            )

-}
oneOfWithFallback :
    List (Renderer (List view -> view))
    -> TagFilter
    -> (String -> view)
    -> (String -> List { name : String, value : String } -> List view -> Maybe view)
    -> Renderer (List view -> view)
oneOfWithFallback decoders (TagFilter tagAllowed) textFn fallbackFn =
    let
        unwrappedDecoders : List (String -> List Markdown.HtmlRenderer.Attribute -> List Block -> Result String (List view -> view))
        unwrappedDecoders =
            decoders
                |> List.map
                    (\(Markdown.HtmlRenderer.HtmlRenderer rawDecoder) -> rawDecoder)
    in
    Markdown.HtmlRenderer.HtmlRenderer
        (\tagName attributes children ->
            let
                specificResult : Result (List String) (List view -> view)
                specificResult =
                    List.foldl
                        (\decoder soFar ->
                            resultOr (decoder tagName attributes children) soFar
                        )
                        (Err [])
                        unwrappedDecoders
            in
            case specificResult of
                Ok view ->
                    Ok view

                Err _ ->
                    if tagAllowed tagName then
                        Ok
                            (\renderedChildren ->
                                case fallbackFn tagName attributes renderedChildren of
                                    Just view ->
                                        view

                                    Nothing ->
                                        textFn (Markdown.HtmlRenderer.htmlElementToString tagName attributes children)
                            )

                    else
                        Ok (\_ -> textFn (Markdown.HtmlRenderer.htmlElementToString tagName attributes children))
        )


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
                            (\{ name } ->
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
