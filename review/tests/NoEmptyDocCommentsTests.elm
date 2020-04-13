module NoEmptyDocCommentsTests exposing (..)

import NoEmptyDocComments
import Review.Test
import Test exposing (..)


suite : Test
suite =
    describe "NoEmptyDocComments"
        [ test "no errors when there's no doc comment" <|
            \() ->
                """module A exposing (..)

a = foo n
"""
                    |> Review.Test.run NoEmptyDocComments.rule
                    |> Review.Test.expectNoErrors
        , test "finds error when there's a blank doc comment" <|
            \() ->
                """module A exposing (..)

{-| This is a module doc -}

import Dict


{-| -}
a = foo n
"""
                    |> Review.Test.run NoEmptyDocComments.rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "It's not done until the docs are great. Or at least present."
                            , details =
                                [ "https://twitter.com/czaplic/status/928359227541798912"
                                ]
                            , under = "{-| -}"
                            }
                        ]
        , test "no error when documentation is present" <|
            \() ->
                """module A exposing (..)

{-| This is a module doc -}

import Dict


{-| I'm a good boy and I wrote some docs -}
a = foo n
"""
                    |> Review.Test.run NoEmptyDocComments.rule
                    |> Review.Test.expectNoErrors
        ]
