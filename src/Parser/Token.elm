module Parser.Token exposing (doubleQuote, hash, newline, singleQuote, space, tab)

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
