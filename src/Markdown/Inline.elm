module Markdown.Inline exposing
    ( Inline(..)
    , extractText
    )

{-| Inline rendering and helpers.


# Model

@docs Inline


# Renders

@docs toHtml, defaultHtml


# Helpers

@docs extractText

-}

import Html exposing (Html, a, br, code, em, img, node, strong, text)
import Html.Attributes exposing (alt, attribute, href, src, title)
import Markdown.Helpers exposing (Attribute)



----------------------------------------------------------------------
-------------------------------- Model -------------------------------
----------------------------------------------------------------------


{-| The inline type.

  - **Text** | _Text_
  - **HardLineBreak**
  - **CodeInline** | _Code_
  - **Link** | _Url_ | _Maybe Title_ | _Inlines_
  - **Image** | _Source_ | _Maybe Title_ | _Inlines_
  - **HtmlInline** | _Tag_ | _List ( Attribute, Maybe Value )_ | _Inlines_
  - **Emphasis** | _Delimiter Length_ | _Inlines_

-}
type Inline
    = Text String
    | HardLineBreak
    | CodeInline String
    | Link String (Maybe String) (List Inline)
    | Image String (Maybe String) (List Inline)
    | HtmlInline String (List ( String, Maybe String )) (List Inline)
    | Emphasis Int (List Inline)



----------------------------------------------------------------------
------------------------ Inline Html Renderer ------------------------
----------------------------------------------------------------------


attributesToHtmlAttributes : List Attribute -> List (Html.Attribute msg)
attributesToHtmlAttributes =
    List.map attributeToAttribute


attributeToAttribute : Attribute -> Html.Attribute msg
attributeToAttribute ( name, maybeValue ) =
    attribute name (Maybe.withDefault name maybeValue)



----------------------------------------------------------------------
------------------------------- Helpers ------------------------------
----------------------------------------------------------------------


{-| Extract the text from a list of inlines.

    inlines : List (Inline)
    inlines =
        [ Text "Heading with "
        , Emphasis 1
            [ Text "emphasis" ]
        ]

    extractText inlines == "Heading with emphasis"

    -- Original string: "Heading with *emphasis*"

-}
extractText : List Inline -> String
extractText inlines =
    List.foldl extractTextHelp "" inlines


extractTextHelp : Inline -> String -> String
extractTextHelp inline text =
    case inline of
        Text str ->
            text ++ str

        HardLineBreak ->
            text ++ " "

        CodeInline str ->
            text ++ str

        Link _ _ inlines ->
            text ++ extractText inlines

        Image _ _ inlines ->
            text ++ extractText inlines

        HtmlInline _ _ inlines ->
            text ++ extractText inlines

        Emphasis _ inlines ->
            text ++ extractText inlines
