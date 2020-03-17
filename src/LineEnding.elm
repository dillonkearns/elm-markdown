module LineEnding exposing (optionalWhitespaceUpToOneLineEnding)

import Helpers
import Parser
import Parser.Advanced as Advanced exposing (..)


type alias Parser a =
    Advanced.Parser String Parser.Problem a


optionalWhitespaceUpToOneLineEnding : Parser ()
optionalWhitespaceUpToOneLineEnding =
    loop CanHaveLineEnding statementsHelp


type State
    = CanHaveLineEnding
    | CannotHaveLineEnding


statementsHelp : State -> Parser (Step State ())
statementsHelp state =
    oneOf
        [ lineEnding
            |> andThen
                (\() ->
                    case state of
                        CanHaveLineEnding ->
                            Loop CannotHaveLineEnding |> succeed

                        CannotHaveLineEnding ->
                            problem (Parser.Problem "Can't have more than one line ending character.")
                )
        , succeed (Loop state)
            |. Advanced.chompIf Helpers.isGfmWhitespace (Parser.Expecting "Whitespace")
        , succeed (Done ())
        ]


{-| From <https://spec.commonmark.org/0.29/#line-ending>:

> is a newline (U+000A), a carriage return (U+000D) not followed by a newline, or a carriage return and a following newline.

-}
lineEnding : Parser ()
lineEnding =
    oneOf
        [ token (toToken "\n")
        , token (toToken (carriageReturn ++ "\n"))
        , token (toToken carriageReturn)
        ]


carriageReturn : String
carriageReturn =
    "\u{000D}"


toToken : String -> Advanced.Token Parser.Problem
toToken str =
    Advanced.Token str (Parser.Expecting str)
