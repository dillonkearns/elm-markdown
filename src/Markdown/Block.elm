module Markdown.Block exposing (Block(..))

import Markdown.CodeBlock
import Markdown.Html exposing (Attribute)
import Markdown.Inlines as Inlines exposing (StyledString)


type Block
    = Heading Int (List StyledString)
    | Body (List StyledString)
    | Html String (List Attribute) (List Block)
    | ListBlock (List (List Inlines.StyledString))
    | CodeBlock Markdown.CodeBlock.CodeBlock
    | ThematicBreak
