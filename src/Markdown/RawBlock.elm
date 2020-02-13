module Markdown.RawBlock exposing (Attribute, RawBlock(..), UnparsedInlines(..))

import Markdown.Block exposing (Block)
import Markdown.CodeBlock exposing (CodeBlock)


type alias Attribute =
    { name : String, value : String }


type UnparsedInlines
    = UnparsedInlines String


type RawBlock
    = Heading Int UnparsedInlines
    | Body UnparsedInlines
    | Html String (List Attribute) (List Block)
    | UnorderedListBlock
        (List
            { task : Maybe Bool
            , body : UnparsedInlines
            }
        )
    | OrderedListBlock Int (List UnparsedInlines)
    | CodeBlock CodeBlock
    | ThematicBreak
    | BlankLine
    | BlockQuote String
