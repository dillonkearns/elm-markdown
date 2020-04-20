module Parser.Token exposing (closingParen, closingSquareBracket, colon, dot, doubleQuote, greaterThan, hash, lessThan, newline, openingSquareBracket, singleQuote, space, tab)

import Parser
import Parser.Advanced as Advanced exposing (Token(..))


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


hash : Token Parser.Problem
hash =
    Token "\n" (Parser.Expecting "a `#`")


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
