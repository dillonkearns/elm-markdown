module ThematicBreak exposing (ThematicBreak(..), parser)

import Parser
import Parser.Advanced as Advanced exposing (..)
import Parser.Extra as Extra exposing (tokenHelp)


type alias Parser a =
    Advanced.Parser String Parser.Problem a


type ThematicBreak
    = ThematicBreak


parser : Parser ThematicBreak
parser =
    succeed identity
        |. oneOf [ chompIf (\c -> c == ' ') (Parser.Expecting "Space"), succeed () ]
        |. oneOf [ chompIf (\c -> c == ' ') (Parser.Expecting "Space"), succeed () ]
        |. oneOf [ chompIf (\c -> c == ' ') (Parser.Expecting "Space"), succeed () ]
        |= Advanced.loop NoMatchYet statementsHelp


type State
    = Asterisk Int
    | Hyphen Int
    | Underscore Int
    | NoMatchYet


type ThematicToken
    = Star
    | Dash
    | TokenUnderscore
    | Finished
    | Whitespace


statementsHelp : State -> Parser (Advanced.Step State ThematicBreak)
statementsHelp state =
    -- Investigate: would splitting this out help?
    -- e.g. oneOf [ whenThematic, whenFinished, whenWhitespace ]
    -- makes the case shorter, and allows `map` instead of `andThen` in some branches
    oneOf
        [ tokenHelp "-" |> map (\_ -> Dash)
        , tokenHelp "*" |> map (\_ -> Star)
        , tokenHelp "_" |> map (\_ -> TokenUnderscore)
        , end (Parser.Expecting "end") |> map (\_ -> Finished)
        , tokenHelp "\n" |> map (\_ -> Finished)
        , chompIf (\c -> c == ' ') (Parser.Expecting "Space") |> map (\_ -> Whitespace)
        ]
        |> andThen
            (\thematicToken ->
                case ( thematicToken, state ) of
                    ( Finished, NoMatchYet ) ->
                        problem (Parser.Expecting "TODO")

                    ( Finished, Asterisk occurrences ) ->
                        succeedIfEnough occurrences

                    ( Finished, Hyphen occurrences ) ->
                        succeedIfEnough occurrences

                    ( Finished, Underscore occurrences ) ->
                        succeedIfEnough occurrences

                    ( Star, Asterisk occurrences ) ->
                        Advanced.Loop (Asterisk (occurrences + 1))
                            |> succeed

                    ( Dash, Hyphen occurrences ) ->
                        Advanced.Loop (Hyphen (occurrences + 1))
                            |> succeed

                    ( TokenUnderscore, Underscore occurrences ) ->
                        Advanced.Loop (Underscore (occurrences + 1))
                            |> succeed

                    ( TokenUnderscore, NoMatchYet ) ->
                        Advanced.Loop (Underscore 1)
                            |> succeed

                    ( Star, NoMatchYet ) ->
                        Advanced.Loop (Asterisk 1)
                            |> succeed

                    ( Dash, NoMatchYet ) ->
                        Advanced.Loop (Hyphen 1)
                            |> succeed

                    ( Whitespace, _ ) ->
                        Advanced.Loop state
                            |> succeed

                    _ ->
                        problem (Parser.Expecting (stateToString state))
            )


succeedIfEnough occurences =
    if occurences > 2 then
        succeed (Done ThematicBreak)

    else
        problem (Parser.Expecting "...?")


stateToString : State -> String
stateToString state =
    case state of
        Asterisk v ->
            "Asterisk " ++ String.fromInt v

        Hyphen v ->
            "Hyphen " ++ String.fromInt v

        Underscore v ->
            "Underscore " ++ String.fromInt v

        NoMatchYet ->
            "NoMatchYet"
