module Parser.Token exposing (asterisk, carriageReturn, closingParen, closingSquareBracket, colon, dot, doubleQuote, greaterThan, hash, lessThan, minus, newline, openingSquareBracket, plus, singleQuote, space, tab, tilde, backtick, equals)

import Parser
import Parser.Advanced exposing (Token(..))


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
    Token "\r" (Parser.Expecting "a carriage return")


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


colon : Token Parser.Problem
colon =
    Token ":" (Parser.Expecting "a `:`")


minus : Token Parser.Problem
minus =
    Token "-" (Parser.Expecting "a `-`")


plus : Token Parser.Problem
plus =
    Token "+" (Parser.Expecting "a `+`")


asterisk : Token Parser.Problem
asterisk =
    Token "*" (Parser.Expecting "a `*`")


equals : Token Parser.Problem
equals =
    Token "=" (Parser.Expecting "a `=`")


tilde : Token Parser.Problem
tilde =
    Token "~" (Parser.Expecting "a `~`")


backtick : Token Parser.Problem
backtick =
    Token "`" (Parser.Expecting "a '`'")
