module Parser.Token exposing (backtick, carriageReturn, closingParen, closingSquareBracket, dot, doubleQuote, equals, greaterThan, hash, lessThan, minus, newline, openingSquareBracket, parseString, singleQuote, space, tab, tilde)

import Parser
import Parser.Advanced as Advanced exposing (Token(..))


type alias Parser a =
    Advanced.Parser String Parser.Problem a


parseString : String -> Parser ()
parseString str =
    Advanced.Token str (Parser.Expecting str)
        |> Advanced.token


singleQuote : Token Parser.Problem
singleQuote =
    Token "'" (Parser.Expecting "a single quote")


doubleQuote : Token Parser.Problem
doubleQuote =
    Token "\"" (Parser.Expecting "a double quote")


tab : Token Parser.Problem
tab =
    Token "\t" (Parser.Expecting "a tab")


space : Token Parser.Problem
space =
    Token " " (Parser.Expecting "a space")


newline : Token Parser.Problem
newline =
    Token "\n" (Parser.Expecting "a newline")


carriageReturn : Token Parser.Problem
carriageReturn =
    Token "\u{000D}" (Parser.Expecting "a carriage return")


hash : Token Parser.Problem
hash =
    Token "#" (Parser.Expecting "a `#`")


greaterThan : Token Parser.Problem
greaterThan =
    Token ">" (Parser.Expecting "a `>`")


lessThan : Token Parser.Problem
lessThan =
    Token "<" (Parser.Expecting "a `<`")


dot : Token Parser.Problem
dot =
    Token "." (Parser.Expecting "a `.`")


closingParen : Token Parser.Problem
closingParen =
    Token ")" (Parser.Expecting "a `)`")


openingSquareBracket : Token Parser.Problem
openingSquareBracket =
    Token "[" (Parser.Expecting "a `[`")


closingSquareBracket : Token Parser.Problem
closingSquareBracket =
    Token "]" (Parser.Expecting "a `]`")


minus : Token Parser.Problem
minus =
    Token "-" (Parser.Expecting "a `-`")


equals : Token Parser.Problem
equals =
    Token "=" (Parser.Expecting "a `=`")


tilde : Token Parser.Problem
tilde =
    Token "~" (Parser.Expecting "a `~`")


backtick : Token Parser.Problem
backtick =
    Token "`" (Parser.Expecting "a '`'")
