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
    -- a thematic break can be preceded by up to 3 spaces.
    -- but in most documents it will be at the start of the line
    -- so we optimize for that case
    oneOf
        [ succeed identity
            |. chompIf isSpace (Parser.Expecting "Space")
            |. oneOf [ chompIf isSpace (Parser.Expecting "Space"), succeed () ]
            |. oneOf [ chompIf isSpace (Parser.Expecting "Space"), succeed () ]
            |= parseThematicBreak
        , parseThematicBreak
        ]


type alias Occurences =
    Int


parseThematicBreak : Parser ThematicBreak
parseThematicBreak =
    oneOf
        [ tokenHelp "-" |> andThen (\_ -> Advanced.loop 1 (statementsHelp (tokenHelp "-")))
        , tokenHelp "*" |> andThen (\_ -> Advanced.loop 1 (statementsHelp (tokenHelp "*")))
        , tokenHelp "_" |> andThen (\_ -> Advanced.loop 1 (statementsHelp (tokenHelp "_")))
        ]


statementsHelp : Parser () -> Occurences -> Parser (Advanced.Step Occurences ThematicBreak)
statementsHelp token occurences =
    oneOf
        [ token |> map (\_ -> Loop (occurences + 1))
        , whitespace occurences
        , end (Parser.Expecting "end") |> andThen (thematicFinish occurences)
        , tokenHelp "\n" |> andThen (thematicFinish occurences)
        ]


thematicFinish : Int -> a -> Parser (Step b ThematicBreak)
thematicFinish occurences _ =
    if occurences > 2 then
        succeed (Done ThematicBreak)

    else
        problem (Parser.Expecting "...?")


whitespace : Occurences -> Parser (Step Occurences a)
whitespace state =
    succeed (Advanced.Loop state)
        |. chompIf isSpace (Parser.Expecting "Space")
        |. chompWhile isSpace


isSpace : Char -> Bool
isSpace c =
    case c of
        ' ' ->
            True

        '\t' ->
            True

        _ ->
            False
