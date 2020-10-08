module Markdown.RawBlock exposing (Attribute, RawBlock(..), UnparsedInlines(..), SetextLevel(..))

import Markdown.Block exposing (Block)
import Markdown.CodeBlock exposing (CodeBlock)
import Markdown.Table
import Markdown.TableParser as TableParser


type alias Attribute =
    { name : String, value : String }


type UnparsedInlines
    = UnparsedInlines String


type SetextLevel
    = LevelOne
    | LevelTwo


type RawBlock
    = Heading Int UnparsedInlines
    | OpenBlockOrParagraph UnparsedInlines
    | Html (Markdown.Block.Html Block)
    | UnorderedListBlock
        (List
            { task : Maybe Bool
            , body : UnparsedInlines
            }
        )
    | OrderedListBlock Int (List UnparsedInlines)
    | CodeBlock CodeBlock
    | IndentedCodeBlock String
    | ThematicBreak
    | Table (Markdown.Table.Table String)
    | TableDelimiter Markdown.Table.TableDelimiterRow
    | BlankLine
    | BlockQuote String
    | SetextLine SetextLevel String
