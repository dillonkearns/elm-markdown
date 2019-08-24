module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Markdown.Parser as Markdown
import Url


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    ()


init : () -> ( Model, Cmd Msg )
init flags =
    ( (), Cmd.none )


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    { title = "elm-markdown-parser"
    , body = [ mainView ]
    }


markdown =
    """# Hello 👋

Welcome to this document!

## Features

Let me tell you why I built this...

<Red>
# Is this red? 😺

It seems to be! 👌
</Red>
"""


mainView : Html msg
mainView =
    Markdown.render markdown
        { h1 = \content -> Html.h1 [] [ Html.text content ]
        , h2 = \content -> Html.h2 [] [ Html.text content ]
        , raw = Html.text
        , todo = Html.text "TODO"
        , red = Html.div [ style "background-color" "red" ]
        }
        |> Result.map (Html.div [])
        |> (\result ->
                case result of
                    Ok content ->
                        content

                    Err error ->
                        error
                            |> Debug.toString
                            |> Html.text
           )
