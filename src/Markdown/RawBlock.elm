module Markdown.RawBlock exposing (Attribute, RawBlock(..), UnparsedInlines(..), SetextLevel(..))

import Markdown.Block exposing (Block)
import Markdown.CodeBlock exposing (CodeBlock)
import Markdown.Table
import Markdown.TableParser as TableParser
import Markdown.UnorderedList exposing (UnorderedListMarker)


type alias Attribute =
    { name : String, value : String }


type UnparsedInlines
    = UnparsedInlines String


type SetextLevel
    = LevelOne
    | LevelTwo


type alias OpenListItem =
    { marker : UnorderedListMarker
    , body : String
    , task : Maybe Bool
    }


type alias CloseListItem =
    { task : Maybe Bool
    , body : List RawBlock
    }


type RawBlock
    = Heading Int UnparsedInlines
    | OpenBlockOrParagraph UnparsedInlines
    | Html (Markdown.Block.Html Block)
    | UnorderedListBlock (List CloseListItem) OpenListItem
    | OrderedListBlock Int (List UnparsedInlines)
    | CodeBlock CodeBlock
    | IndentedCodeBlock String
    | ThematicBreak
    | Table (Markdown.Table.Table String)
    | TableDelimiter Markdown.Table.TableDelimiterRow
    | BlankLine
    | BlockQuote String
    | ParsedBlockQuote (List RawBlock)
    | SetextLine SetextLevel String
