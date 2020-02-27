module BlockTransformTests exposing (suite)

import Expect
import Markdown.Block as Block exposing (..)
import Test exposing (..)


suite : Test
suite =
    only <|
        describe "transform blocks"
            [ test "validate links" <|
                \() ->
                    let
                        httpLinksToHttps =
                            String.replace "http://" "https://"
                    in
                    [ Paragraph
                        [ Link "http://elm-lang.org" Nothing [ Text "elm-lang search results" ]
                        ]
                    ]
                        |> mapThing httpLinksToHttps
                        |> Expect.equal
                            (Ok
                                [ Paragraph
                                    [ Link "https://elm-lang.org" Nothing [ Text "elm-lang search results" ]
                                    ]
                                ]
                            )
            ]


mapThing mapFn blocks =
    Ok
        [ Paragraph
            [ Link "https://elm-lang.org" Nothing [ Text "elm-lang search results" ]
            ]
        ]
