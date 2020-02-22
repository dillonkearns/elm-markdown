module HtmlNodeToHtml exposing (..)

import HtmlParser exposing (Node(..))
import Markdown.Block as Block exposing (Block, Inline, ListItem, Task)
import Markdown.RawBlock exposing (Attribute, RawBlock(..), UnparsedInlines(..))
import Parser.Advanced as Advanced exposing ((|.), (|=), Nestable(..), Step(..), succeed)


fromNode =
    \xmlNode ->
        case xmlNode of
            HtmlParser.Text innerText ->
                -- TODO is this right?
                Block.HtmlComment innerText

            HtmlParser.Element tag attributes children ->
                Block.HtmlElement tag attributes children

            Comment string ->
                Block.HtmlComment string

            Cdata string ->
                Block.Cdata string

            ProcessingInstruction string ->
                Block.ProcessingInstruction string

            Declaration declarationType content ->
                Block.HtmlDeclaration declarationType content
