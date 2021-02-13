module Markdown.CodeBlock exposing (..)

import Helpers
import Parser
import Parser.Advanced as Advanced exposing (..)
import Parser.Token as Token
import Parser.Extra as Extra
import Whitespace


type alias Parser a =
    Advanced.Parser String Parser.Problem a


type alias CodeBlock =
    { language : Maybe String
    , body : String
    }


type alias Fence =
    { character : FenceCharacterConfig
    , length : Int
    , indented : Int
    }


type FenceCharacter
    = Backtick
    | Tilde


type alias FenceCharacterConfig =
    { kind : FenceCharacter, char : Char, token : Token Parser.Problem }


backtick =
    { kind = Backtick, char = '`', token = Token.backtick }


tilde =
    { kind = Tilde, char = '~', token = Token.tilde }


--


parser : Parser CodeBlock
parser =
    openingFence
        |> andThen
            (\fence ->
                succeed CodeBlock
                    |= infoString fence.character
                    |. Helpers.lineEndOrEnd
                    |= remainingBlock fence
            )


-- Parses the opening fence and with it information about
-- indentation, and what the closing fence should look like.
openingFence : Parser Fence
openingFence =
    succeed (\indent ( character, length ) -> { character = character, length = length, indented = indent })
        |. Whitespace.upToThreeSpaces
        -- Indentation
        |= ( getCol |> andThen colToIndentation )
        |= oneOf
            [ fenceOfAtLeast 3 backtick
            , fenceOfAtLeast 3 tilde
            ]


-- In this case max three, as that's the max the opening fence can be indented.
-- getCol always starts from 1, so 1 needs to be subtracted.
colToIndentation : Int -> Parser Int
colToIndentation int =
    case int of
        1 -> succeed 0
        2 -> succeed 1
        3 -> succeed 2
        4 -> succeed 3
        _ -> Advanced.problem (Parser.Expecting "Fenced code blocks should be indented no more than 3 spaces")


-- Parses the closing fence, making sure it is the right length
closingFence : Int -> FenceCharacterConfig -> Parser ()
closingFence minLength fenceCharacter =
    succeed ()
        |. Whitespace.upToThreeSpaces
        |. fenceOfAtLeast minLength fenceCharacter
        |. chompWhile Whitespace.isSpace
        |. Helpers.lineEndOrEnd


{- Code fence (from GFM):
    > sequence of at least three consecutive backtick characters (`) or tildes (~).
    > (Tildes and backticks cannot be mixed.)
-}
fenceOfAtLeast : Int -> FenceCharacterConfig -> Parser ( FenceCharacterConfig, Int )
fenceOfAtLeast minLength fenceCharacter =
    let
        -- Chains the token parsers together
        -- End up with succeed () |. token Token.tilde |. ... for minLength
        builtTokens =
            token fenceCharacter.token
                |> List.repeat minLength
                |> List.foldl (\t p -> p |. t) (succeed ())
    in
    builtTokens
        -- As long as we have minLength tokens, the rest are ok
        |. chompWhile ((==) fenceCharacter.char)
        |> mapChompedString (\str _ -> ( fenceCharacter, String.length str ))


{- Info string
    > The line with the opening code fence may optionally be followed by text.
    > This is trimmed of leading and trailing whitespace.
    > If coming after a ` fence, it must not contain any ` characters.
-}
infoString : FenceCharacterConfig -> Parser (Maybe String)
infoString fenceCharacter =
    let
        toInfoString str _ =
            case String.trim str of
                "" ->
                    Nothing

                trimmed ->
                    Just trimmed
    in
    case fenceCharacter.kind of
        Backtick ->
            chompWhile (\c -> c /= '`' && not (Whitespace.isLineEnd c))
                |> mapChompedString toInfoString

        Tilde ->
            chompWhile (not << Whitespace.isLineEnd)
                |> mapChompedString toInfoString


{- Body
    > If the leading code fence is indented N spaces, then up to N spaces of
    > indentation are removed from each line of the content (if present).
-}
type alias Body =
    String


remainingBlock : Fence -> Parser Body
remainingBlock fence =
    loop ( fence, "" ) remainingBlockHelp


remainingBlockHelp : ( Fence, Body ) -> Parser (Step ( Fence, Body ) Body)
remainingBlockHelp ( fence, body ) =
    oneOf
        -- End of the string
        [ succeed (Done body)
            |. Advanced.end Parser.ExpectingEnd

        -- End of the line, chomps up the line ending
        , Whitespace.lineEnd
            |> mapChompedString (\lineEnd _ -> Loop ( fence, body ++ lineEnd ))

        -- Closing fence
        , succeed (Done body)
            |. closingFence fence.length fence.character
            |> Advanced.backtrackable

        -- A code line. `codeBlockLine` returns the offset after any extra indendation
        -- is collapsed. The line is then sliced and added to the body. This is the
        -- equivalent to `mapChompedString` with a custom start value.
        , succeed (\start end source -> Loop ( fence, body ++ String.slice start end source ))
            |= codeBlockLine fence.indented
            |= getOffset
            |= getSource
        ]


-- Parse code block line, returning the offset at which the indentation ended
codeBlockLine : Int -> Parser Int
codeBlockLine indented =
    succeed identity
        |. Extra.upTo indented Whitespace.space
        |= getOffset
        |. Helpers.chompUntilLineEndOrEnd
        |. Helpers.lineEndOrEnd
